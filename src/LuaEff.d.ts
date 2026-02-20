// Strictly internal type brands
declare const ComputationBrand: unique symbol;
declare const FOBrand: unique symbol;
declare const HOBrand: unique symbol;
declare const FORecordBrand: unique symbol;
declare const HORecordBrand: unique symbol;

declare const _: unique symbol;
export interface _ {
  [_]: typeof _;
}

export type Resume<T> = (value: T) => Computation<any, any, any>;

/**
 * A first-order effect.
 * @param {Payload} Payload The type of the value to be supplied to the effect.
 * @param {Resume} Resume The type of the value passed into the next continuation.
 */
export interface FOEffect<Payload, Resume> {
  readonly payload: Payload;
  readonly resume: Resume;
}

/**
 * A higher-order effect.
 * @param {Payload} Payload Type of the value to be supplied to the effect.
 * @param {SubComputations} SubComputations Tuple type of the computations to be elaborated.
 * @param {Resume} Resume Type of the value passed into the next continuation.
 */
export interface HOEffect<
  Payload,
  SubComputations extends
    | { [key: string]: Computation<any, any, any> }
    | { [key: number]: Computation<any, any, any> },
  Resume,
> {
  readonly payload: Payload;
  readonly subcomputations: SubComputations;
  readonly resume: Resume;
}

/**
 * First-order effect definitions.
 * @param {T} T Generic type parameter for effects that allow polymorphism.
 * Effects polymorphic in multiple type parameters should instead use a wrapper around `op`.
 */
export interface FOEffects<T> {
  io: FOEffect<() => T, T>;
  fail: FOEffect<undefined, never>;
  async: FOEffect<() => Promise<T>, T>;
  ndet: FOEffect<null | number | ReadonlyArray<unknown>, unknown>;
}

type BuiltinFOEffects = "io" | "fail" | "async" | "ndet";

/**
 * Higher-order effect definitions.
 * @param {T} T Generic type parameter for higher-order effects that allow polymorphism.
 * Effects polymorphic in multiple type parameters should instead use a wrapper around `hOp`.
 */
export interface HOEffects<T> {
  parallel: HOEffect<
    undefined,
    { [K in keyof T]: Computation<any, any, T[K]> },
    T
  >;
  foo: HOEffect<T, [AnyComputation], T>;
}

type AllFOEffects = keyof FOEffects<any>;
type AllHOEffects = keyof HOEffects<any>;
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

/** Lifts a value into a computation. */
export declare function ret<A>(value: A): Pure<A>;

export declare function op<Name extends AllFOEffects, T>(
  effect: Name,
  payload: FOPayloadOf<Name, T>,
): Computation<never, Name, FOResumeOf<Name, T>>;

export declare function hOp<Name extends AllHOEffects, T>(
  effect: Name,
  payload: HOResumeOf<Name, T>,
  subcomputations: HOSubComputationsOf<Name, T>,
): Computation<Name, never, HOResumeOf<Name, T>>;

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
    ? Substitute<B, _, A>
    : A;

type FOPayloadOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["payload"];

type FOResumeOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["resume"];

type FOHandlerOf<Name extends AllFOEffects, T> = (
  payload: FOPayloadOf<Name, T>,
  resume: Resume<FOResumeOf<Name, T>>,
) => AnyComputation;

type HOPayloadOf<Name extends AllHOEffects, T> = HOEffects<T>[Name]["payload"];

type HOSubComputationsOf<
  Name extends AllHOEffects,
  T,
> = HOEffects<T>[Name]["subcomputations"];

type HOResumeOf<Name extends AllHOEffects, T> = HOEffects<T>[Name]["resume"];

type HOElaboratorOf<Name extends AllHOEffects, T> = (
  payload: HOPayloadOf<Name, T>,
  subcomputations: HOSubComputationsOf<Name, T>,
  resume: HOResumeOf<Name, T>,
) => AnyComputation;

type CoercePure<C extends Computation<any, any, any>> =
  C extends Computation<never, never, infer A> ? Pure<A> : C;

type CoerceComputation<T> =
  T extends Computation<never, never, infer A>
    ? Pure<A>
    : T extends Computation<infer HO, infer FO, infer A>
      ? Computation<HO, FO, A>
      : T;

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
    computation: Computation<HO, FO, A>,
  ): Computation<
    | Exclude<HO, ElaboratedEffects>
    | ArrayUnion<
        IntroducedHOEffects["record"][HO extends unknown
          ? keyof HOEffects<any>
          : HO]
      >,
    | FO
    | ArrayUnion<
        IntroducedFOEffects["record"][HO extends unknown
          ? keyof HOEffects<any>
          : HO]
      >,
    A
  >;
}

interface FOIntroductionRecord {
  record: Record<AllFOEffects, any[]>;
  [FORecordBrand]: typeof FORecordBrand;
}
type HOIntroductionRecord = {
  record: Record<AllHOEffects, any[]>;
  [HORecordBrand]: typeof HORecordBrand;
};

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
  record: {
    [Name in keyof R["record"]]: R["record"][Name] extends Array<infer A>
      ? [...A[], V]
      : never;
  };
  [FORecordBrand]: typeof FORecordBrand;
};

type HOKeyAddAll<R extends HOIntroductionRecord, V extends AllHOEffects> = {
  record: {
    [Name in keyof R["record"]]: R["record"][Name] extends Array<infer A>
      ? [...A[], V]
      : never;
  };
  [HORecordBrand]: typeof HORecordBrand;
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
  handle<Name extends AllFOEffects, T = unknown>(
    effect: Name,
    handler: FOHandlerOf<Name, T>,
  ): Handler<
    HandledEffects | Name,
    ReturnType<typeof handler> extends Computation<infer HO, any, any>
      ? FOKeyAdd<IntroducedHOEffects, Name, HO>
      : never,
    ReturnType<typeof handler> extends Computation<any, infer FO, any>
      ? FOKeyAdd<IntroducedFOEffects, Name, FO>
      : never,
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
  elaborate<Name extends AllHOEffects, T>(
    effect: Name,
    elaborator: HOElaboratorOf<Name, T>,
  ): Elaborator<
    ElaboratedEffects | Name,
    ReturnType<typeof elaborator> extends Computation<infer HO, any, any>
      ? HOKeyAdd<IntroducedHOEffects, Name, HO>
      : never,
    ReturnType<typeof elaborator> extends Computation<any, infer FO, any>
      ? HOKeyAdd<IntroducedFOEffects, Name, FO>
      : never
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
  PickOne<T> extends infer U
    ? Exclude<T, U> extends never
      ? [T]
      : [...UnionToTuple<Exclude<T, U>>, U]
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
