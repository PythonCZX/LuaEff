import {
  run,
  Computation,
  seq,
  ret,
  Resume,
  AnyComputation,
  Handler,
  op,
  _,
  Has,
  AllHOEffects,
  AllFOEffects,
  do_,
  hOp,
  Elaborator,
  pipe,
  Pure,
  HOEffectHKT,
  FOEffect,
  HOEffectsOf,
  FOEffectsOf,
  ResultOf,
} from "./LuaEffTesting";

// ═══════════════════════════════════════════════════════════════════════════
// Module Augmentation: First-Order Effects
// ═══════════════════════════════════════════════════════════════════════════

declare module "./LuaEffTesting" {
  interface FOEffects<T> {
    throw: FOEffect<string, never>;
    beginTransaction: FOEffect<void, number>;
    commit: FOEffect<number, void>;
    rollback: FOEffect<number, void>;
    connect: FOEffect<void, number>;
    disconnect: FOEffect<number, void>;
    query: FOEffect<string, QueryResult>;
    insert: FOEffect<{ tables: string; values: number }, string>;
    update: FOEffect<{ tables: string; id: string; values: number }, boolean>;
  }
  interface HOEffectHKTs {
    catch: CatchHKT;
    transact: TransactHKT;
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// Module Augmentation: Higher-Order Effect HKTs
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Catch effect: tries the first computation, falls back to second on throw.
 *
 * - Payload: void
 * - SubComputations: exactly two computations with the same result type
 * - Resume: the result type of the subcomputations
 */
interface CatchHKT extends HOEffectHKT {
  readonly payload: void;

  readonly resume: this["subComputations"] extends readonly [
    infer C1 extends AnyComputation,
    infer C2 extends AnyComputation,
  ]
    ? ResultOf<C1> // Both should have same result type
    : never;

  readonly type: this["subComputations"] extends readonly [
    infer C1 extends AnyComputation,
    infer C2 extends AnyComputation,
  ]
    ? Computation<
        "catch" | HOEffectsOf<C1> | HOEffectsOf<C2>,
        FOEffectsOf<C1> | FOEffectsOf<C2>,
        ResultOf<C1>
      >
    : never;
}

/**
 * Transact effect: wraps a computation in a database transaction.
 *
 * - Payload: void
 * - SubComputations: exactly one computation
 * - Resume: the result type of the subcomputation
 */
interface TransactHKT extends HOEffectHKT {
  readonly payload: void;

  readonly resume: this["subComputations"] extends readonly [
    infer C extends AnyComputation,
  ]
    ? ResultOf<C>
    : never;

  readonly type: this["subComputations"] extends readonly [
    infer C extends AnyComputation,
  ]
    ? Computation<"transact" | HOEffectsOf<C>, FOEffectsOf<C>, ResultOf<C>>
    : never;
}

// ═══════════════════════════════════════════════════════════════════════════
// Domain Types
// ═══════════════════════════════════════════════════════════════════════════

type QueryResult = {
  rows: Array<{
    id: number;
    name: string;
  }>;
};

// ═══════════════════════════════════════════════════════════════════════════
// Effect Operations
// ═══════════════════════════════════════════════════════════════════════════

const log = (msg: string) => op<"io", void>("io", () => print(msg));

const throw_ = (msg: string) => op("throw", msg);

/**
 * Catch operation: try the first computation, run the second on failure.
 *
 * Note: The type parameter T is gone. The result type is inferred from
 * the subcomputations. Both must have compatible result types.
 */
const catch_ = <
  C1 extends AnyComputation,
  C2 extends Computation<any, any, ResultOf<C1>>,
>(
  tryComp: C1,
  handlerComp: C2,
) => hOp("catch", undefined, [tryComp, handlerComp] as const);

/**
 * Transact operation: wrap a computation in a transaction.
 */
const transact = <C extends AnyComputation>(computation: C) =>
  hOp("transact", undefined, [computation] as const);

// ═══════════════════════════════════════════════════════════════════════════
// Handlers & Elaborators
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Creates a handler runner that intercepts 'throw' effects.
 * On throw, runs handlerBlock then continues with k.
 */
function runThrow(
  handlerBlock: AnyComputation,
  k: (v: unknown) => AnyComputation,
) {
  return new Handler()
    .handle("throw", (_payload, _resume) => seq(handlerBlock, k))
    .mapReturn((x: _) => k(x))
    .build().run;
}

/**
 * Elaborator for 'catch': converts the HO effect into throw handling.
 *
 * The elaborator receives Pure subcomputations (effects already handled
 * in the try/handler blocks for purposes of the elaborator's view).
 */
const runCatch = new Elaborator()
  .elaborate<"catch", [unknown, unknown], never, never, unknown>(
    "catch",
    (_payload, [tryComp, handlerComp], resume) => {
      return runThrow(handlerComp, resume)(tryComp);
    },
  )
  .build().run;

/**
 * Elaborator for 'transact': wraps computation in begin/commit/rollback.
 *
 * On success: commits the transaction
 * On throw: rolls back and logs failure
 */
const runTransaction = new Elaborator()
  .elaborate<
    "transact",
    [unknown],
    "catch",
    "beginTransaction" | "commit" | "rollback" | "io",
    unknown
  >("transact", (_payload, [body], resume) => {
    // Inner computation: run body then commit
    const c1 = (txId: number) =>
      seq(body, result => seq(op("commit", txId), () => resume(result)));

    // Handler computation: rollback and log on failure
    const c2 = (txId: number) =>
      seq(op("rollback", txId), () => log("Transaction failed!"));

    // Wrap in catch: begin transaction, try c1, fall back to c2
    const res = runCatch(
      seq(op("beginTransaction", undefined), txId =>
        catch_(c1(txId), c2(txId)),
      ),
    );
    return res;
  })
  .build().run;

// ═══════════════════════════════════════════════════════════════════════════
// Database Handler
// ═══════════════════════════════════════════════════════════════════════════

const delay = (ms: number) =>
  op<"async", void>(
    "async",
    () => new Promise(resolve => task.delay(ms / 1000, resolve)),
  );

function runDatabase() {
  let data: Map<string, number> = new Map();
  let connections = 0;
  const transactions: Map<number, Map<string, number>> = new Map();
  let txCounter = 0;

  return new Handler()
    .handle("connect", (_p, resume) => {
      connections++;
      const connId = connections;
      return seq(log(`[DB] Connection ${connId} opened`), () => resume(connId));
    })
    .handle("disconnect", (connId, resume) =>
      seq(log(`[DB] Connection ${connId} closed`), () => resume()),
    )
    .handle("query", (sql, resume) =>
      seq(log(`[DB] Query: ${sql}`), () =>
        seq(delay(500), () => {
          const result: QueryResult = { rows: [{ id: 1, name: "Alice" }] };
          return seq(log(`[DB] Query result: ${result}`), () => resume(result));
        }),
      ),
    )
    .handle("insert", ({ tables, values }, resume) =>
      seq(log(`[DB] Insert into ${tables}: ${values}`), () =>
        seq(delay(500), () => {
          const id = game
            .GetService("HttpService")
            .GenerateGUID(false)
            .sub(1, 9);
          data.set(id, values);
          return seq(log(`[DB] Inserted with id: ${id}`), () => resume(id));
        }),
      ),
    )
    .handle("update", ({ tables, id, values }, resume) =>
      seq(log(`[DB] Update ${tables} id=${id}: ${values}`), () =>
        seq(delay(500), () => {
          if (data.has(id)) {
            data.set(id, tonumber(`${data.get(id)!}${values}`)!);
            return seq(log(`[DB] Updated id: ${id}`), () => resume(true));
          }
          return resume(false);
        }),
      ),
    )
    .handle("beginTransaction", (_p, resume) => {
      const txId = ++txCounter;
      transactions.set(txId, new Map(data)); // Clone current state
      return seq(log(`[DB] Transaction ${txId} started`), () => resume(txId));
    })
    .handle("commit", (txId, resume) => {
      if (transactions.has(txId)) {
        return seq(log(`[DB] Transaction ${txId} committed`), () => {
          transactions.delete(txId);
          return resume();
        });
      }
      return resume();
    })
    .handle("rollback", (txId, resume) => {
      if (transactions.has(txId)) {
        const tx = transactions.get(txId)!;
        data = tx; // Restore to snapshot
        return seq(log(`[DB] Transaction ${txId} rolled back`), () => {
          transactions.delete(txId);
          return resume();
        });
      }
      return resume();
    })
    .build().run;
}

// ═══════════════════════════════════════════════════════════════════════════
// Program
// ═══════════════════════════════════════════════════════════════════════════

const program = transact(
  do_(function* () {
    const id1: string = yield op("insert", { tables: "users", values: 40 });
    const id2: string = yield op("insert", { tables: "users", values: 50 });
    yield throw_("constraint-violation");
    return [id1, id2];
  }),
);

// Run the program through the transaction elaborator
const A = runTransaction(program);

// To fully execute, also need database handler and other effect handlers:
// const result = pipe(
//   program,
//   runTransaction,
//   runDatabase(),
//   run,
// );
