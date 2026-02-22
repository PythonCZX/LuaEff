// ═══════════════════════════════════════════════════════════════════════════
// SECTION 1: Computation Types (unchanged, needed for HKT definitions)
// ═══════════════════════════════════════════════════════════════════════════

declare const ComputationBrand: unique symbol;

declare const _: unique symbol;
export interface _ {
  [_]: typeof _;
}

export type Resume<T> = (value: T) => Pure<unknown>;

interface Computation<
  HO extends AllHOEffects = never,
  FO extends AllFOEffects = never,
  A = unknown,
> {
  readonly [ComputationBrand]: {
    readonly hoEffects: HO;
    readonly foEffects: FO;
    readonly result: A;
  };
}

export type AnyComputation = Computation<any, any, any>;
export type Pure<A> = Computation<never, never, A>;

export type HOEffectsOf<C> =
  C extends Computation<infer HO, any, any> ? HO : never;

export type FOEffectsOf<C> =
  C extends Computation<any, infer FO, any> ? FO : never;

export type AllEffectsOf<C> =
  C extends Computation<infer HO, infer FO, any> ? HO | FO : never;

export type ResultOf<C> = C extends Computation<any, any, infer A> ? A : never;

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 2: First-Order Effects (unchanged)
// ═══════════════════════════════════════════════════════════════════════════

export interface FOEffect<Payload, Resume> {
  readonly payload: Payload;
  readonly resume: Resume;
}

export interface FOEffects<T> {
  io: FOEffect<() => T, T>;
  fail: FOEffect<undefined, never>;
  async: FOEffect<() => Promise<T>, T>;
  ndet: FOEffect<null | number | ReadonlyArray<unknown>, unknown>;
}

type AllFOEffects = keyof FOEffects<any>;
type BuiltinFOEffects = "io" | "fail" | "async" | "ndet";

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 3: HKT Infrastructure
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Base interface for higher-order effect HKTs.
 *
 * Each HO effect is encoded as an HKT that computes its output type
 * based on the provided subcomputations. The `subComputations` field
 * acts as the "input slot" that gets filled when the HKT is applied.
 */
interface HOEffectHKT {
  /** Input slot: tuple of subcomputations passed to hOp */
  readonly subComputations: unknown;
  /** Payload type required by the effect (may depend on subComputations) */
  readonly payload: unknown;
  /** Resume type passed to the continuation (derived from subComputations) */
  readonly resume: unknown;
  /** Output: the resulting Computation type */
  readonly type: AnyComputation;
}

/**
 * Apply an HO effect HKT to concrete subcomputations.
 * This "fills the slot" and extracts the computed type.
 */
type ApplyHOEffect<F extends HOEffectHKT, Subs> = (F & {
  readonly subComputations: Subs;
})["type"];

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 4: Sub-Effect Lifting Helpers
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Collect all higher-order effects from a tuple of computations into a union.
 */
type UnionHOOf<Subs> = Subs extends readonly (infer C)[]
  ? C extends AnyComputation
    ? HOEffectsOf<C>
    : never
  : never;

/**
 * Collect all first-order effects from a tuple of computations into a union.
 */
type UnionFOOf<Subs> = Subs extends readonly (infer C)[]
  ? C extends AnyComputation
    ? FOEffectsOf<C>
    : never
  : never;

/**
 * Map a tuple of computations to a tuple of their result types.
 * Preserves tuple structure (length, labels).
 */
type ResultsOf<Subs> = {
  [K in keyof Subs]: Subs[K] extends AnyComputation ? ResultOf<Subs[K]> : never;
};

/**
 * Map a tuple of computations to a tuple of Pure computations with same results.
 * Used for elaborator subcomputation arguments.
 */
type ToPure<Subs> = {
  [K in keyof Subs]: Subs[K] extends AnyComputation
    ? Pure<ResultOf<Subs[K]>>
    : never;
};

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 5: Individual Higher-Order Effect HKTs
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Parallel effect: runs multiple computations concurrently.
 *
 * - Payload: void (no additional data needed)
 * - SubComputations: any tuple of computations
 * - Resume: tuple of all subcomputation results
 * - Propagates all effects from subcomputations
 */
interface ParallelHKT extends HOEffectHKT {
  readonly payload: void;

  readonly resume: this["subComputations"] extends readonly AnyComputation[]
    ? ResultsOf<this["subComputations"]>
    : never;

  readonly type: this["subComputations"] extends readonly AnyComputation[]
    ? Computation<
        "parallel" | UnionHOOf<this["subComputations"]>,
        UnionFOOf<this["subComputations"]>,
        ResultsOf<this["subComputations"]>
      >
    : never;
}

/**
 * Foo effect: example effect with a single subcomputation.
 *
 * - Payload: same type as the subcomputation's result
 * - SubComputations: exactly one computation
 * - Resume: the subcomputation's result type
 *
 * This demonstrates fixed-arity effects where we index positionally.
 */
interface FooHKT extends HOEffectHKT {
  readonly payload: this["subComputations"] extends readonly [
    infer C extends AnyComputation,
    ...any[],
  ]
    ? ResultOf<C>
    : unknown;

  readonly resume: this["subComputations"] extends readonly [
    infer C extends AnyComputation,
    ...any[],
  ]
    ? ResultOf<C>
    : never;

  readonly type: this["subComputations"] extends readonly [
    infer C extends AnyComputation,
  ]
    ? Computation<"foo" | HOEffectsOf<C>, FOEffectsOf<C>, ResultOf<C>>
    : never;
}

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 6: HKT Registry
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Registry mapping effect names to their HKTs.
 * Adding a new HO effect requires:
 * 1. Define an interface extending HOEffectHKT
 * 2. Add it to this registry
 */
interface HOEffectHKTs {
  parallel: ParallelHKT;
  foo: FooHKT;
}

type AllHOEffects = keyof HOEffectHKTs;
type AllEffects = AllHOEffects | AllFOEffects;

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 7: Helper Types for hOp
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Extract payload type for an effect given concrete subcomputations.
 * For effects like `parallel`, this is constant (void).
 * For effects like `foo`, this depends on subcomputation results.
 */
type HOPayloadOf<Name extends AllHOEffects, Subs> = (HOEffectHKTs[Name] & {
  readonly subComputations: Subs;
})["payload"];

/**
 * Extract resume type for an effect given concrete subcomputations.
 */
type HOResumeOf<Name extends AllHOEffects, Subs> = (HOEffectHKTs[Name] & {
  readonly subComputations: Subs;
})["resume"];

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 8: Core Operations
// ═══════════════════════════════════════════════════════════════════════════

/** Lifts a value into a pure computation. */
export declare function ret<A>(value: A): Pure<A>;

/** First-order effect operation (unchanged) */
type FOPayloadOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["payload"];
type FOResumeOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["resume"];

export declare function op<Name extends AllFOEffects, T>(
  effect: Name,
  payload: FOPayloadOf<Name, T>,
): Computation<never, Name, FOResumeOf<Name, T>>;

/**
 * Higher-order effect operation.
 *
 * The polymorphism previously threaded through T is now computed from
 * the actual subcomputations provided. This gives better inference:
 * the result type is determined by what you pass in, not what you
 * declare upfront.
 */
export declare function hOp<
  Name extends AllHOEffects,
  Subs extends readonly AnyComputation[],
>(
  effect: Name,
  payload: HOPayloadOf<Name, Subs>,
  subcomputations: Subs,
): ApplyHOEffect<HOEffectHKTs[Name], Subs>;

/** Binds the output of the first computation to the given function. */
export declare function seq<
  HO1 extends AllHOEffects,
  FO1 extends AllFOEffects,
  A,
  HO2 extends AllHOEffects,
  FO2 extends AllFOEffects,
  B,
>(
  current: Computation<HO1, FO1, A>,
  then: (value: A) => Computation<HO2, FO2, B>,
): Computation<HO1 | HO2, FO1 | FO2, B>;

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 9: Elaborator Types
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Type of an elaborator function for a specific effect.
 *
 * The elaborator is generic in Results (the tuple of result types from
 * subcomputations). This replaces the old T parameter with something
 * more precise: we know exactly what shape of results to expect.
 *
 * The subcomputations argument receives Pure versions since effects
 * have already been elaborated away by the time the elaborator runs.
 */
type HOElaboratorOf<
  Name extends AllHOEffects,
  Results extends readonly unknown[],
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = (
  payload: HOPayloadOfResults<Name, Results>,
  subcomputations: { [K in keyof Results]: Pure<Results[K]> },
  resume: Resume<HOResumeOfResults<Name, Results>>,
) => Computation<HO, FO, A>;

/**
 * Compute payload type from result types (for elaborator signatures).
 * This mirrors what HOPayloadOf does, but takes Results directly
 * instead of Subs.
 */
type HOPayloadOfResults<
  Name extends AllHOEffects,
  Results,
> = Name extends "parallel"
  ? void
  : Name extends "foo"
    ? Results extends readonly [infer T, ...any[]]
      ? T
      : never
    : never;

/**
 * Compute resume type from result types (for elaborator signatures).
 */
type HOResumeOfResults<
  Name extends AllHOEffects,
  Results,
> = Name extends "parallel"
  ? Results
  : Name extends "foo"
    ? Results extends readonly [infer T, ...any[]]
      ? T
      : never
    : never;

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 10: Handler & Elaborator Classes
// ═══════════════════════════════════════════════════════════════════════════

// Brand symbols for type-level records
declare const FORecordBrand: unique symbol;
declare const HORecordBrand: unique symbol;

interface FOIntroductionRecord {
  record: Record<AllFOEffects, any[]>;
  [FORecordBrand]: typeof FORecordBrand;
}

interface HOIntroductionRecord {
  record: Record<AllHOEffects, any[]>;
  [HORecordBrand]: typeof HORecordBrand;
}

type FOKeyAdd<
  R extends FOIntroductionRecord,
  K extends AllFOEffects,
  V extends AllEffects,
> = K extends keyof FOIntroductionRecord
  ? {
      record: {
        [Name in keyof R["record"]]: K extends Name
          ? R["record"][Name] extends Array<infer A>
            ? [...A[], V]
            : never
          : R["record"][Name];
      };
      [FORecordBrand]: typeof FORecordBrand;
    }
  : never;

type FOKeyAddAll<R extends FOIntroductionRecord, V extends AllFOEffects> = {
  record: {
    [Name in keyof R["record"]]: R["record"][Name] extends Array<infer A>
      ? [...A[], V]
      : never;
  };
  [FORecordBrand]: typeof FORecordBrand;
};

type HOKeyAdd<
  R extends HOIntroductionRecord,
  K extends AllHOEffects,
  V extends AllEffects,
> = K extends keyof HOEffectHKTs
  ? K extends keyof HOIntroductionRecord["record"]
    ? {
        record: {
          [Name in keyof R["record"]]: K extends Name
            ? R["record"][Name] extends Array<infer A>
              ? [...A[], V]
              : never
            : R["record"][Name];
        };
        [HORecordBrand]: typeof HORecordBrand;
      }
    : never
  : never;

type HOKeyAddAll<R extends HOIntroductionRecord, V extends AllHOEffects> = {
  record: {
    [Name in keyof R["record"]]: R["record"][Name] extends Array<infer A>
      ? [...A[], V]
      : never;
  };
  [HORecordBrand]: typeof HORecordBrand;
};

type ArrayUnion<Arr extends any[]> = Arr extends [...infer Tail, infer Head]
  ? Head | ArrayUnion<Tail>
  : never;

type CoercePure<C extends Computation<any, any, any>> =
  C extends Computation<never, never, infer A> ? Pure<A> : C;

type ApplyReturnTransform<F, A> = F extends undefined
  ? A
  : F extends (x: _) => Computation<any, any, infer B>
    ? Substitute<B, _, A>
    : A;

// Substitute helper (unchanged from original)
type ReplaceInTuple<T extends any[], TReplace, TWith> = T extends []
  ? []
  : T extends [infer Head, ...infer Tail]
    ? [
        Substitute<Head, TReplace, TWith>,
        ...ReplaceInTuple<Tail, TReplace, TWith>,
      ]
    : { [K in keyof T]: Substitute<T[K], TReplace, TWith> };

type Substitute<T, TReplace, TWith> = T extends TReplace
  ? TWith | Exclude<T, TReplace>
  : T extends Computation<infer HO, infer FO, infer A>
    ? Computation<HO, FO, Substitute<A, TReplace, TWith>>
    : T extends (...args: infer Args) => infer R
      ? (
          ...args: ReplaceInTuple<
            Args extends any[] ? Args : never,
            TReplace,
            TWith
          >
        ) => Substitute<R, TReplace, TWith>
      : { [P in keyof T]: Substitute<T[P], TReplace, TWith> };

export interface RunHandler<
  HandledEffects extends AllFOEffects,
  IntroducedHOEffects extends FOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord,
  Transform,
> {
  run<HO extends AllHOEffects, FO extends AllFOEffects, A>(
    this: void,
    computation: Computation<HO, FO, A>,
  ): CoercePure<
    Computation<
      HO | ArrayUnion<IntroducedHOEffects["record"][FO]>,
      | Exclude<FO, HandledEffects>
      | ArrayUnion<IntroducedFOEffects["record"][FO]>,
      ApplyReturnTransform<Transform, A>
    >
  >;
}

export interface RunElaborator<
  ElaboratedEffects extends AllHOEffects,
  IntroducedHOEffects extends HOIntroductionRecord,
  IntroducedFOEffects extends HOIntroductionRecord,
> {
  run<HO extends AllHOEffects, FO extends AllFOEffects, A>(
    this: void,
    computation: Computation<HO, FO, A>,
  ): Computation<
    | Exclude<HO, ElaboratedEffects>
    | ArrayUnion<IntroducedHOEffects["record"][HO]>,
    FO | ArrayUnion<IntroducedFOEffects["record"][HO]>,
    A
  >;
}

export declare class Handler<
  HandledEffects extends AllFOEffects = never,
  IntroducedHOEffects extends FOIntroductionRecord = FOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord = FOIntroductionRecord,
  Transform = undefined,
> {
  handle<
    Name extends AllFOEffects,
    T,
    HO extends AllHOEffects,
    FO extends AllFOEffects,
    A,
  >(
    effect: Name,
    handler: (
      payload: FOPayloadOf<Name, T>,
      resume: Resume<FOResumeOf<Name, T>>,
    ) => Computation<HO, FO, A>,
  ): Handler<
    HandledEffects | Name,
    FOKeyAdd<IntroducedHOEffects, Name, HO>,
    FOKeyAdd<IntroducedFOEffects, Name, FO>,
    Transform
  >;

  mapReturn<F extends (x: any) => Computation<any, any, any>>(
    f: F,
  ): Handler<
    HandledEffects,
    FOKeyAddAll<IntroducedHOEffects, HOEffectsOf<ReturnType<F>>>,
    FOKeyAddAll<IntroducedFOEffects, FOEffectsOf<ReturnType<F>>>,
    F
  >;

  build(): RunHandler<
    HandledEffects,
    IntroducedHOEffects,
    IntroducedFOEffects,
    Transform
  >;
}

export declare class Elaborator<
  ElaboratedEffects extends AllHOEffects = never,
  IntroducedHOEffects extends HOIntroductionRecord = HOIntroductionRecord,
  IntroducedFOEffects extends HOIntroductionRecord = HOIntroductionRecord,
> {
  /**
   * Register an elaborator for a higher-order effect.
   *
   * The elaborator function is generic in Results, allowing it to handle
   * any valid subcomputation tuple for the effect. The HO/FO/A parameters
   * capture what effects the elaborator body introduces.
   */
  elaborate<
    Name extends AllHOEffects,
    Results extends readonly unknown[],
    HO extends AllHOEffects,
    FO extends AllFOEffects,
    A,
  >(
    effect: Name,
    elaborator: HOElaboratorOf<Name, Results, HO, FO, A>,
  ): Elaborator<
    ElaboratedEffects | Name,
    HOKeyAdd<IntroducedHOEffects, Name, HO>,
    HOKeyAdd<IntroducedFOEffects, Name, FO>
  >;

  build(): RunElaborator<
    ElaboratedEffects,
    IntroducedHOEffects,
    IntroducedFOEffects
  >;
}

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 11: Runtime & Utilities
// ═══════════════════════════════════════════════════════════════════════════

// Error formatting (unchanged, just references new AllHOEffects)
type Contra<T> = T extends any ? (arg: T) => void : never;
type Cov<T> = T extends any ? () => T : never;
type InferCov<T> = [T] extends [() => infer I] ? I : never;
type InferContra<T> = [T] extends [(arg: infer I) => void] ? I : never;
type PickOne<T> = InferContra<InferContra<Contra<Contra<T>>>>;

type UnionToTuple<T> =
  PickOne<T> extends infer U
    ? Exclude<T, U> extends never
      ? [T]
      : [...UnionToTuple<Exclude<T, U>>, U]
    : never;

type FormatUnion<U extends string> = UnionToTuple<U>;
type TupleToString<A extends string[]> = A extends [
  infer Head1,
  infer Head2,
  ...infer Tail,
]
  ? Tail extends [string, ...string[]]
    ? `${Head1 & string}, ${Head2 & string}, ${TupleToString<Tail>}`
    : `${Head1 & string}, ${Head2 & string}`
  : A extends [infer Head]
    ? `${Head & string}`
    : "";

type FormatFO<FO extends AllFOEffects> =
  FormatUnion<Exclude<FO, BuiltinFOEffects>> extends [never]
    ? ""
    : TupleToString<FormatUnion<Exclude<FO, BuiltinFOEffects>> & string[]>;

type AllEffectsHandled<HO, FO> = HO extends never
  ? Exclude<FO, BuiltinFOEffects> extends never
    ? true
    : false
  : false;

type UnhandledEffectError<
  HO extends AllHOEffects,
  FO extends AllFOEffects,
> = `Unhandled higher-order effects: [${TupleToString<FormatUnion<HO & string> & string[]>}] | Unhandled first-order effects: [${FormatFO<FO>}]`;

type RuntimeResult<FO extends AllFOEffects, A> = "async" extends FO
  ? Promise<A>
  : "ndet" extends FO
    ? A[]
    : A;

export declare function run<
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
>(
  computation: Computation<HO, FO, A>,
): AllEffectsHandled<HO, FO> extends true
  ? RuntimeResult<FO, A>
  : UnhandledEffectError<HO, FO>;

// Pipe and Do notation (unchanged)
export declare function pipe<A>(a: A): A;
export declare function pipe<A, B>(a: A, ab: (a: A) => B): B;
export declare function pipe<A, B, C>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
): C;
export declare function pipe<A, B, C, D>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
): D;
export declare function pipe<A, B, C, D, E>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
): E;
export declare function pipe<A, B, C, D, E, F>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
): F;
export declare function pipe<A, B, C, D, E, F, G>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
): G;
export declare function pipe<A, B, C, D, E, F, G, H>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
  gh: (g: G) => H,
): H;
export declare function pipe<A, B, C, D, E, F, G, H, I>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
  gh: (g: G) => H,
  hi: (h: H) => I,
): I;
export declare function pipe<A, B, C, D, E, F, G, H, I, J>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
  gh: (g: G) => H,
  hi: (h: H) => I,
  ij: (i: I) => J,
): J;

export type DoGenerator<
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = Generator<Computation<HO, FO, any>, A, any>;

export declare function do_<
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
>(genFn: () => DoGenerator<HO, FO, A>): Computation<HO, FO, A>;

// ═══════════════════════════════════════════════════════════════════════════
// SECTION 12: Constraint Helpers
// ═══════════════════════════════════════════════════════════════════════════

export type Has<C extends Computation<any, any, any>, E extends AllEffects> =
  Exclude<E, Extract<AllEffectsOf<C>, E>> extends never ? C : never;

export type FOComputation<FO extends AllFOEffects, A> = Computation<
  never,
  FO,
  A
>;
