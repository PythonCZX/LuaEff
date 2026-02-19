// Strictly internal type brands
declare const ComputationBrand: unique symbol;
declare const FOBrand: unique symbol;
declare const HOBrand: unique symbol;

declare const _: unique symbol;
export interface _ {
  [_]: typeof _;
}

export type Resume<T> = (value: T) => Computation<any, any, any>;

/**
 * A first-order effect token to be used
 * with `op` to define operations.
 */
export type FOOperation<Name extends AllFOEffects> = (
  payload: FOPayloadOf<Name>,
) => Computation<never, Name, FOResumeOf<Name>>;

/**
 * A higher-order effect token to be used
 * with `hOp` to define higher-order operations.
 */
export type HOOperation<Name extends AllHOEffects> = (
  payload: HOPayloadOf<Name>,
  subcomputations: HOSubComputationsOf<Name>,
) => Computation<Name, never, HOResumeOf<Name>>;

/**
 * First-order effect definitions.
 * @param {T} T Generic type parameter for effects that allow polymorphism.
 * Effects polymorphic in multiple type parameters should instead use a wrapper around `op`.
 */
// export interface FOEffects<T> {
//   io: FOEffect<() => T, T>;
//   fail: FOEffect<undefined, never>;
//   async: FOEffect<() => Promise<T>, T>;
//   ndet: FOEffect<null | number | ReadonlyArray<unknown>, unknown>;
// }

export interface FOEffects<R extends AnyComputation = AnyComputation> {
  io: <T>(payload: () => T, resume: Resume<T>) => R;
  fail: (payload: undefined, resume: Resume<never>) => R;
  async: <T>(payload: () => Promise<T>, resume: Resume<T>) => R;
  ndet: (
    payload: null | number | ReadonlyArray<unknown>,
    resume: Resume<unknown>,
  ) => R;
}

type BuiltinFOEffects = "io" | "fail" | "async" | "ndet";

/**
 * Higher-order effect definitions.
 * @param {T} T Generic type parameter for higher-order effects that allow polymorphism.
 * Effects polymorphic in multiple type parameters should instead use a wrapper around `hOp`.
 */
export interface HOEffects<R extends AnyComputation = AnyComputation> {
  parallel: <Ts extends unknown[]>(
    payload: undefined,
    subcomputations: { [K in keyof Ts]: Computation<any, any, Ts> },
    resume: Resume<Ts>,
  ) => R;
}

type AllFOEffects = keyof FOEffects;
type AllHOEffects = keyof HOEffects;
type AllEffects = AllHOEffects | AllFOEffects;

/**
 * A computation to be handled, elaborated, or run.
 * @param {HO} HO Union of higher-order effect names.
 * @param {FO} FO Union of first-order effect names.
 * @param {A} A Result type of the computation.
 */

interface Computation<
  HO extends AllHOEffects = never,
  FO extends AllFOEffects = never,
  A = unknown,
> {
  /** @internal */
  readonly [ComputationBrand]: {
    readonly hoEffects: HO;
    readonly foEffects: FO;
    readonly result: A;
  };
}

export type AnyComputation = Computation<any, any, any>;

/** A computation without effects. */
export type Pure<A> = Computation<never, never, A>;

/** Extract the higher-order effect row from a computation type. */
export type HOEffectsOf<C> =
  C extends Computation<infer HO, any, any> ? HO : never;

/** Extract the first-order effect row from a computation type. */
export type FOEffectsOf<C> =
  C extends Computation<any, infer FO, any> ? FO : never;

export type AllEffectsOf<C> =
  C extends Computation<infer HO, infer FO, any> ? HO | FO : never;

/** Extract the result type from a computation type. */
export type ResultOf<C> = C extends Computation<any, any, infer A> ? A : never;

/**
 * Asserts that computation `C` carries every effect in the union `E`.
 * the registries. Evaluates to `C` on success, `never` on failure.
 *
 * @example
 *   function needsState<HO extends string, FO extends string, A>(
 *     c: Has<Computation<HO, FO, A>, "io" | "state">
 *   ): void {}
 */
export type Has<C extends Computation<any, any, any>, E extends AllEffects> =
  Exclude<E, Extract<AllEffectsOf<C>, E>> extends never ? C : never;

/**
 * A computation that carries only first-order effects.
 */
export type FOComputation<FO extends AllFOEffects, A> = Computation<
  never,
  FO,
  A
>;

export declare const opIO: FOOperation<"io">;
export declare const opFAIL: FOOperation<"fail">;
export declare const opASYNC: FOOperation<"async">;
export declare const opNDET: FOOperation<"ndet">;

/** Lifts a value into a computation. */
export declare function ret<A>(value: A): Pure<A>;

export declare function op<Name extends AllFOEffects>(
  effect: Name,
): FOOperation<Name>;

export declare function hOp<Name extends AllHOEffects>(
  effect: Name,
): HOOperation<Name>;

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

export type ApplyReturnTransform<F, A> = F extends undefined
  ? A
  : F extends (x: _) => Computation<any, any, infer B>
    ? Replaced<B, _, A>
    : A;

type FOPayloadOf<Name extends AllFOEffects> = FOEffects[Name] extends (
  payload: infer P,
  resume: any,
) => AnyComputation
  ? P
  : never;

type FOResumeOf<Name extends AllFOEffects> = FOEffects[Name] extends (
  payload: any,
  resume: (x: infer R) => AnyComputation,
) => AnyComputation
  ? R
  : never;

type FOHandlerOf<
  Name extends AllFOEffects,
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = FOEffects<Computation<HO, FO, A>>[Name];

type HOPayloadOf<Name extends AllHOEffects> = HOEffects[Name] extends (
  payload: infer P,
  subcomputations: any,
  resume: any,
) => AnyComputation
  ? P
  : never;

type HOSubComputationsOf<Name extends AllHOEffects> = HOEffects[Name] extends (
  payload: any,
  subcomputations: infer S,
  resume: any,
) => AnyComputation
  ? S
  : never;

type HOResumeOf<Name extends AllHOEffects> = HOEffects[Name] extends (
  payload: any,
  subcomputations: any,
  resume: (x: infer R) => AnyComputation,
) => AnyComputation
  ? R
  : never;

type HOElaboratorOf<
  Name extends AllHOEffects,
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = HOEffects<Computation<HO, FO, A>>[Name];

type CoercePure<C extends Computation<any, any, any>> =
  C extends Computation<never, never, infer A> ? Pure<A> : C;

// export type EffectHandler<
//   HandledEffects extends AllFOEffects,
//   IntroducedHOEffects extends HOIntroductionRecord,
//   IntroducedFOEffects extends FOIntroductionRecord,
//   Transform,
//   HO extends AllHOEffects,
//   FO extends AllFOEffects,
//   A,
// > = (
//   computation: Computation<HO, FO, A>,
// ) => Computation<
//   HO | IntroducedHOEffects[FO],
//   Exclude<FO, HandledEffects> | IntroducedFOEffects[FO],
//   ApplyReturnTransform<Transform, A>
// >;

type CoerceComputation<T> =
  T extends Computation<never, never, infer A>
    ? Pure<A>
    : T extends Computation<infer HO, infer FO, infer A>
      ? Computation<HO, FO, A>
      : T;

export type Replaced<T, TReplace, TWith> = T extends TReplace
  ? T extends TReplace
    ? TWith | Exclude<T, TReplace>
    : T
  : T extends (...args: any) => infer R
    ? (...args: Parameters<T>) => Replaced<R, TReplace, TWith>
    : CoerceComputation<{
        [P in keyof T]: Replaced<T[P], TReplace, TWith>;
      }>;

export interface RunHandler<
  HandledEffects extends AllFOEffects,
  IntroducedHOEffects extends FOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord,
  Transform,
> {
  run<HO extends AllHOEffects, FO extends AllFOEffects, A>(
    computation: Computation<HO, FO, A>,
  ): CoercePure<
    Computation<
      HO | ArrayUnion<IntroducedHOEffects[FO]>,
      Exclude<FO, HandledEffects> | ArrayUnion<IntroducedFOEffects[FO]>,
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
    computation: Computation<HO, FO, A>,
  ): Computation<
    | Exclude<HO, ElaboratedEffects>
    | ArrayUnion<
        IntroducedHOEffects[HO extends unknown ? keyof HOEffects<any> : HO]
      >,
    | FO
    | ArrayUnion<
        IntroducedFOEffects[HO extends unknown ? keyof HOEffects<any> : HO]
      >,
    A
  >;
}

type FOIntroductionRecord = Record<AllFOEffects, any[]>;
type HOIntroductionRecord = Record<AllHOEffects, any[]>;

type FOKeyAdd<
  R extends FOIntroductionRecord,
  K extends AllFOEffects,
  V extends AllEffects,
> = K extends keyof FOIntroductionRecord
  ? {
      [Name in keyof R]: K extends Name
        ? R[Name] extends Array<infer A>
          ? [...A[], V]
          : never
        : R[Name];
    }
  : R & { [key in K]: [] };

type FOKeyAddAll<R extends FOIntroductionRecord, V extends AllFOEffects> = {
  [Name in keyof R]: R[Name] extends Array<infer A> ? [...A[], V] : never;
};

type HOKeyAddAll<R extends HOIntroductionRecord, V extends AllHOEffects> = {
  [Name in keyof R]: R[Name] extends Array<infer A> ? [...A[], V] : never;
};

type HOKeyAdd<
  R extends HOIntroductionRecord,
  K extends AllHOEffects,
  V extends AllEffects,
> = K extends keyof HOEffects<any>
  ? K extends keyof HOIntroductionRecord
    ? {
        [Name in keyof R]: K extends Name
          ? R[Name] extends Array<infer A>
            ? [...A[], V]
            : never
          : R[Name];
      }
    : R & { [key in K]: [] }
  : never;

type ArrayUnion<Arr extends any[]> = Arr extends [...infer Tail, infer Head]
  ? Head | ArrayUnion<Tail>
  : never;

export declare class Handler<
  HandledEffects extends AllFOEffects = never,
  IntroducedHOEffects extends FOIntroductionRecord = FOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord = FOIntroductionRecord,
  Transform = undefined,
> {
  handle<
    Name extends AllFOEffects,
    HO extends AllHOEffects = never,
    FO extends AllFOEffects = never,
  >(
    effect: Name,
    handler: FOHandlerOf<Name, HO, FO, any>,
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
  IntroducedHOEffects extends HOIntroductionRecord = never,
  IntroducedFOEffects extends HOIntroductionRecord = never,
> {
  elaborate<
    Name extends AllHOEffects,
    HO extends AllHOEffects,
    FO extends AllFOEffects,
  >(
    effect: HOOperation<Name>,
    elaborator: HOElaboratorOf<Name, HO, FO, any>,
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

type Contra<T> = T extends any ? (arg: T) => void : never;

type Cov<T> = T extends any ? () => T : never;

type InferCov<T> = [T] extends [() => infer I] ? I : never;

type InferContra<T> = [T] extends [(arg: infer I) => void] ? I : never;

type PickOne<T> = InferContra<InferContra<Contra<Contra<T>>>>;

type UnionToTuple<T> =
  PickOne<T> extends infer U // assign PickOne<T> to U
    ? Exclude<T, U> extends never // T and U are the same
      ? [T]
      : [...UnionToTuple<Exclude<T, U>>, U] // recursion
    : never;

type AllEffectsHandled<HO, FO> = HO extends never
  ? Exclude<FO, BuiltinFOEffects> extends never
    ? true
    : false
  : false;

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

// Clean up type names
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
