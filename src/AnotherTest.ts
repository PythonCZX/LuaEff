import {
  run,
  Computation,
  seq,
  ret,
  AnyComputation,
  op,
  AllHOEffects,
  AllFOEffects,
  do_,
  hOp,
  pipe,
  Pure,
  FOEffect,
  HOEffectsOf,
  FOEffectsOf,
  ResultOf,
  handles,
  elaborates,
  HandlerHKT,
  withHandler,
  WithHandler,
  HandledEffectsFromHKT,
  IntroducedFOEffectsFromHandlerHKTFor,
  IntroducedFOEffectsFromHandlerReturn,
  HOEffect,
  Just,
  SubHOEffects,
  SubFOEffects,
  ZipN,
  ElaboratorHKT,
  withElaborator,
  SubComputationBrand,
  SubComputationMap,
  SubComputationEntry,
  UnionToTuple,
  PickOne,
} from "./LuaEff";
import { BuiltinFOEffects } from "./LuaEffTesting";

interface Fiber {
  readonly _tag: "Fiber";
  readonly cancel: () => void;
  readonly result: Promise<Exit>;
}

type Exit =
  | { readonly _tag: "Success"; readonly value: unknown }
  | { readonly _tag: "Interrupted" };

declare module "./LuaEff" {
  interface FOEffects<T> {
    get: FOEffect<void, T>;
    set: FOEffect<T, void>;
    interrupt: FOEffect<Fiber, Exit>;
  }
  interface HOEffects<T> {
    fork: HOEffect<void, readonly [Exit], Fiber>;
  }
}

const get = <T = number>() => op<"get", T>("get", undefined);
const set = <T = number>(v: T) => op<"set", T>("set", v);

class StateHandler<S, T, K> {
  constructor(private state: S) {
    this.state = state;
  }

  @handles("get")
  get(_p: void, resume: (x: S) => K) {
    return resume(this.state);
  }

  @handles("set")
  set(v: S, resume: () => K) {
    this.state = v;
    return resume();
  }

  return(x: T) {
    return ret([x, this.state] as readonly [T, S]);
  }
}

interface StateHKT<S> extends HandlerHKT {
  readonly type: StateHandler<
    S,
    this["computationReturn"],
    this["continuation"]
  >;
}

type EJE = WithHandler<h, p>;

type p = typeof program;
type h = StateHKT<number>;

function runState<S>(initial: S) {
  return function <C extends AnyComputation>(comp: C) {
    return withHandler<StateHKT<S>>(new StateHandler(initial)).run(comp);
  };
}

const x = 1;

const program = do_(function* () {
  const a = yield* get();
  const b = 2 * a;
  yield* set(b);
  return "Done.";
});

const program2 = do_(function* () {
  for (let i = 0; i < 10; i++) {
    yield* op(
      "async",
      () => new Promise(resolve => task.delay(2, resolve as Callback)),
    );
    yield* op("io", () => print("This is actually working???"));
  }
});

function divide(a: number, b: number) {
  return do_(function* () {
    if (b === 0) {
      yield* op("fail", undefined);
    } else {
      return a / b;
    }
  });
}

const program3 = do_(function* () {
  const [a, b] = yield* get<readonly [number, number]>();
  return yield* divide(a, b);
});

const sleep = (ms: number) =>
  op<"async", void>(
    "async",
    () => new Promise(resolve => task.delay(ms / 1000, resolve as Callback)),
  );

const log = (msg: string) => op<"io", void>("io", () => print(msg));

const interrupt = (fiber: Fiber) => op("interrupt", fiber);

function forever<C extends AnyComputation>(
  comp: C,
): Computation<HOEffectsOf<C>, FOEffectsOf<C>, never> {
  return seq(comp, () => forever(comp));
}

const fork = <C extends AnyComputation>(comp: C) =>
  hOp<"fork", [HOEffectsOf<C>], [FOEffectsOf<C>]>("fork", undefined, [comp]);

class InterruptHandler<K> {
  @handles("interrupt")
  interrupt(fiber: Fiber, resume: (v: Exit) => K) {
    fiber.cancel();
    return seq(
      op<"async", Exit>("async", () => fiber.result),
      (exit: Exit) => resume(exit),
    );
  }
}

interface InterruptHKT extends HandlerHKT {
  readonly type: InterruptHandler<this["continuation"]>;
}

class ForkElaborator<
  SubHO extends SubHOEffects,
  SubFO extends SubFOEffects,
  K,
> {
  constructor(
    private readonly runSub: (comp: AnyComputation) => Promise<unknown>,
  ) {}

  @elaborates("fork")
  fork(
    _: void,
    [comp]: ZipN<1, SubHO, SubFO, Exit>,
    resume: (fiber: Fiber) => K,
  ) {
    let canceled = false;
    let resolveCancel: (exit: Exit) => void;
    const cancelPromise = new Promise<Exit>(resolve => {
      resolveCancel = resolve;
    });

    const resultPromise: Promise<Exit> = Promise.race([
      this.runSub(comp).andThen(
        (v): Exit =>
          canceled ? { _tag: "Interrupted" } : { _tag: "Success", value: v },
      ),
      cancelPromise,
    ]);

    const fiber: Fiber = {
      _tag: "Fiber",
      cancel: () => {
        canceled = true;
        resolveCancel({ _tag: "Interrupted" });
      },
      result: resultPromise,
    };
    return resume(fiber);
  }
}

type Vals<T extends readonly any[]> = T[0];

interface ForkElaboratorHKT extends ElaboratorHKT {
  readonly type: ForkElaborator<
    this["higherOrderEffects"],
    this["firstOrderEffects"],
    this["continuation"]
  >;
  readonly requires: BuiltinFOEffects;
}

function runInterrupt<C extends AnyComputation>(comp: C) {
  return withHandler<InterruptHKT>(new InterruptHandler()).run(comp);
}

function runSubs(comp: AnyComputation): Promise<unknown> {
  return run(runInterrupt(comp));
}

const parallel = <C extends AnyComputation>(comp: C) =>
  hOp<"foo", [HOEffectsOf<C>], [FOEffectsOf<C>], number>("foo", 1, [comp]);

const choose = (choices: ReadonlyArray<string>) =>
  op("ndet", choices) as Computation<never, "ndet", string>;
const program4 = do_(function* () {
  const s = yield* choose(["Hello", "world.", "This"]);
  const fiber = yield* fork(
    forever(
      do_(function* () {
        yield* log(s);
        yield* sleep(500);
      }),
    ),
  );
  yield* sleep(5000);
  const exit = yield* interrupt(fiber);
  print(exit);
});

const firstInter = withElaborator<ForkElaboratorHKT>(
  new ForkElaborator(runSubs),
).run(program4);
const secondInter = runInterrupt(firstInter);
const lastInter = run(secondInter);
