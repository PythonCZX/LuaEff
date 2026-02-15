// Strictly internal type brands
declare const ComputationBrand: unique symbol;
declare const FOBrand: unique symbol;
declare const HOBrand: unique symbol;

/**
 * A first-order effect token to be used
 * with `op` to define operations.
 */
export type FOToken<Name extends AllFOEffects> = {
  readonly id: number;
  readonly name: Name;
  /** @internal */ readonly [FOBrand]: Name;
};

/**
 * A higher-order effect token to be used
 * with `hOp` to define higher-order operations.
 */
export interface HOToken<Name extends AllHOEffects> {
  readonly id: number;
  readonly name: Name;
  /** @internal */ readonly [HOBrand]: Name;
}

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
  SubComputations extends { [key: string]: Computation<any, any, any> },
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

/**
 * Higher-order effect definitions.
 * @param {T} T Generic type parameter for higher-order effects that allow polymorphism.
 * Effects polymorphic in multiple type parameters should instead use a wrapper around `hOp`.
 */
export interface HOEffects<T> {}

type AllFOEffects = keyof FOEffects<any>;
type AllHOEffects = keyof HOEffects<any> extends never
  ? any
  : keyof HOEffects<any>;
type AllEffects = keyof HOEffects<any> extends never
  ? AllFOEffects
  : AllHOEffects | AllFOEffects;

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

export declare const IO: FOToken<"io">;
export declare const FAIL: FOToken<"fail">;
export declare const ASYNC: FOToken<"async">;
export declare const NDET: FOToken<"ndet">;

/** Creates a first-order effect token */
export declare function newEffect<Name extends AllFOEffects>(
  name: Name,
): FOToken<Name>;

/** Creates a higher-order effect token */
export declare function newEffect<Name extends AllHOEffects>(
  name: Name,
): HOToken<Name>;

/** Lifts a value into a computation. */
export declare function ret<A>(value: A): Pure<A>;

/**
 * Performs a first-order effect.
 * `T` is inferred from the payload of the corresponding
 * effect in `FOEffects`.
 */
export declare function op<Name extends AllFOEffects, T = unknown>(
  effect: FOToken<Name>,
  payload?: FOEffects<T>[Name]["payload"],
): Computation<never, Name, FOEffects<T>[Name]["resume"]>;

/**
 * Performs a higher-order effect.
 * `T` is inferred from the payload of the corresponding
 * effect in `HOEffects`.
 */
export declare function hOp<Name extends AllHOEffects, T = unknown>(
  effect: HOToken<Name>,
  payload?: HOEffects<T>[keyof HOEffects<any> extends never
    ? never
    : Name]["payload"],
  subcomputations?: HOEffects<T>[keyof HOEffects<any> extends never
    ? never
    : Name]["subcomputations"],
): Computation<
  Name,
  never,
  HOEffects<T>[keyof HOEffects<any> extends never ? never : Name]["resume"]
>;

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

type ApplyReturnTransform<F, A> = [F] extends [undefined]
  ? A
  : F extends (x: A) => Computation<any, any, infer B>
    ? B
    : A;

type FOPayloadOf<
  Name extends AllFOEffects,
  T = unknown,
> = Name extends keyof FOEffects<T> ? FOEffects<T>[Name]["payload"] : never;

type FOResumeOf<
  Name extends AllFOEffects,
  T = unknown,
> = Name extends keyof FOEffects<T> ? FOEffects<T>[Name]["resume"] : never;

type HOPayloadOf<
  Name extends AllHOEffects,
  T = unknown,
> = Name extends keyof HOEffects<T> ? HOEffects<T>[Name]["payload"] : never;

type HOSubComputationsOf<
  Name extends AllHOEffects,
  T = unknown,
> = Name extends keyof HOEffects<T>
  ? HOEffects<T>[Name]["subcomputations"]
  : never;

type HOResumeOf<
  Name extends AllHOEffects,
  T = unknown,
> = Name extends keyof HOEffects<T> ? HOEffects<T>[Name]["resume"] : never;

export type EffectHandler<
  HandledEffects extends AllFOEffects,
  IntroducedHOEffects extends HOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord,
  Transform,
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = (
  computation: Computation<HO, FO, A>,
) => Computation<
  HO | IntroducedHOEffects[FO],
  Exclude<FO, HandledEffects> | IntroducedFOEffects[FO],
  ApplyReturnTransform<Transform, A>
>;

export type EffectElaborator<
  ElaboratedEffects extends AllHOEffects,
  IntroducedFOEffects extends AllFOEffects,
  IntroducedHOEffects extends AllHOEffects,
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = (
  computation: Computation<HO, FO, A>,
) => Computation<
  Exclude<HO, ElaboratedEffects> | IntroducedHOEffects,
  FO | IntroducedFOEffects,
  A
>;

type FOIntroductionRecord = Record<AllFOEffects, any>;
type HOIntroductionRecord = Record<AllHOEffects, any>;

type FOKeyAdd<
  R extends FOIntroductionRecord,
  K extends AllFOEffects,
  V extends AllFOEffects,
> = K extends keyof FOIntroductionRecord
  ? {
      [Name in keyof R]: K extends Name
        ? R[Name] extends Array<infer A>
          ? [...A[], V]
          : never
        : R[Name];
    }
  : R & { [key in K]: [] };

type HOKeyAdd<
  R extends HOIntroductionRecord,
  K extends AllHOEffects,
  V extends AllHOEffects,
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
  IntroducedHOEffects extends HOIntroductionRecord = HOIntroductionRecord,
  IntroducedFOEffects extends FOIntroductionRecord = FOIntroductionRecord,
  Transform = undefined,
> {
  handle<
    T,
    Name extends AllFOEffects,
    HO extends AllHOEffects,
    FO extends AllFOEffects,
  >(
    effect: FOToken<Name>,
    handler: (
      payload: FOPayloadOf<Name, T>,
      resume: (value: FOResumeOf<Name, T>) => Computation<any, any, any>,
    ) => Computation<HO, FO, any>,
  ): Handler<
    HandledEffects | Name,
    HOKeyAdd<IntroducedHOEffects, Name, HO>,
    FOKeyAdd<IntroducedFOEffects, Name, Exclude<FO, Name>>,
    Transform
  >;

  mapReturn<F extends (x: any) => Computation<any, any, any>>(
    f: F,
  ): Handler<HandledEffects, IntroducedHOEffects, IntroducedFOEffects, F>;

  build<HO extends AllHOEffects, FO extends AllFOEffects, A>(): EffectHandler<
    HandledEffects,
    IntroducedHOEffects,
    IntroducedFOEffects,
    Transform,
    HO,
    FO,
    A
  >;
}

// export declare class Elaborator<ElaboratedEffects extends AllHOEffects = never, IntroducedHOEffects extends AllHOEffects = never, IntroducedFOEffects extends AllFOEffects = never> {
//   handle<Name extends AllHOEffects, HO extends AllHOEffects, FO extends AllFOEffects>(
//     effect: HOToken<Name>,
//     handler: (
//       payload: HOPayloadOf<Name>,
//       subcomputations: HOSubComputationsOf<Name>,
//       resume: (
//         value: HOResumeOf<Name>
//       ) =>
//     )
//   )
// }
