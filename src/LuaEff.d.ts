declare const ComputationBrand: unique symbol;
declare const SubComputationBrand: unique symbol;

interface FOEffect<Payload, Resume> {
  readonly payload: Payload;
  readonly resume: Resume;
}

interface HOEffect<
  Payload,
  SubResults extends { readonly [key: number]: unknown },
  Resume,
> {
  readonly payload: Payload;
  readonly results: SubResults;
  readonly resume: Resume;
}

interface FOEffects<T> {
  io: FOEffect<() => T, T>;
  fail: FOEffect<undefined, never>;
  async: FOEffect<() => Promise<T>, T>;
  ndet: FOEffect<null | number | ReadonlyArray<unknown>, unknown>;
}

interface HOEffects<T> {
  parallel: HOEffect<void, { [K in keyof T]: T[K] }, T>;
  foo: HOEffect<T, [T], T>;
}

type BuiltinFOEffects = "io" | "fail" | "async" | "ndet";
type AllFOEffects = keyof FOEffects<any>;
type AllHOEffects = keyof HOEffects<any>;
type SubHOEffects = readonly AllHOEffects[];
type SubFOEffects = readonly AllFOEffects[];
type AllEffects = AllHOEffects | AllFOEffects;

type SubComputationEntry = {
  readonly subHO: SubHOEffects;
  readonly subFO: SubFOEffects;
};

type SubComputationMap = {
  readonly [Name in AllHOEffects]?: SubComputationEntry;
};

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

type AnyComputation = Computation<any, any, any>;
type Pure<A> = Computation<never, never, A>;
type FOComputation<FO extends AllFOEffects, A> = Computation<never, FO, A>;
type DoGenerator<
  HO extends AllHOEffects,
  FO extends AllFOEffects,
  A,
> = Generator<Computation<HO, FO, any>, A, any>;

type HOEffectsOf<C> = C extends Computation<infer HO, any, any> ? HO : never;
type FOEffectsOf<C> = C extends Computation<any, infer FO, any> ? FO : never;
type AllEffectsOf<C> =
  C extends Computation<infer HO, infer FO, any> ? HO | FO : never;
type ResultOf<C> = C extends Computation<any, any, infer A> ? A : never;

type Has<C extends Computation<any, any, any>, E extends AllEffects> =
  Exclude<E, Extract<AllEffectsOf<C>, E>> extends never ? C : never;

type FOPayloadOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["payload"];
type FOResumeOf<Name extends AllFOEffects, T> = FOEffects<T>[Name]["resume"];
type HOPayloadOf<Name extends AllHOEffects, T> = HOEffects<T>[Name]["payload"];
type HOSubResultsOf<
  Name extends AllHOEffects,
  T,
> = HOEffects<T>[Name]["results"];
type HOResumeOf<Name extends AllHOEffects, T> = HOEffects<T>[Name]["resume"];
type MapComputation<T> = {
  readonly [K in keyof T]: Computation<any, any, T[K]>;
};
type HOSubComputationsOf<Name extends AllHOEffects, T> = MapComputation<
  HOSubResultsOf<Name, T>
>;

type MergeSubComputations<A, B> = (A extends {
  readonly [SubComputationBrand]: infer subA;
}
  ? subA
  : {}) &
  (B extends { readonly [SubComputationBrand]: infer subB } ? subB : {});

type Sequence<Current, Then> = Computation<
  HOEffectsOf<Current> | HOEffectsOf<ReturnType<Then>>,
  FOEffectsOf<Current> | FOEffectsOf<ReturnType<Then>>,
  ResultOf<ReturnType<Then>>
> & {
  readonly [SubComputationBrand]: MergeSubComputations<
    Current,
    ReturnType<Then>
  >;
};

interface HandlerHKT {
  readonly computationReturn: unknown;
  readonly continuation: Computation<any, any, unknown>;
}

interface ElaboratorHKT {
  readonly computationReturn: unknown;
  readonly higherOrderEffects: SubHOEffects;
  readonly firstOrderEffects: SubFOEffects;
  readonly continuation: Computation<any, any, unknown>;
}

type ApplyHandlerHKT<F, Return, Continuation> = F extends {
  readonly type: unknown;
}
  ? (F & {
      readonly computationReturn: Return;
      readonly continuation: Continuation;
    })["type"]
  : never;

type ApplyElaboratorHKT<F, Return, HO, FO, Continuation> = F extends {
  readonly type: unknown;
}
  ? (F & {
      readonly computationReturn: Return;
      readonly higherOrderEffects: HO;
      readonly firstOrderEffects: FO;
      readonly continuation: Continuation;
    })["type"]
  : never;

type TransformHandlerReturn<HKT extends HandlerHKT, A> =
  ApplyHandlerHKT<HKT, A, Pure<A>> extends { return: (x: A) => infer R }
    ? R extends Computation<any, any, infer B>
      ? B
      : R
    : A;

type HandledEffectsFromHKT<HKT extends HandlerHKT> =
  ApplyHandlerHKT<HKT, unknown, unknown> extends infer H
    ? { [K in keyof H]: K extends AllFOEffects ? K : never }[keyof H]
    : never;

type ElaboratedEffectsFromHKT<HKT extends ElaboratorHKT> =
  ApplyElaboratorHKT<HKT, unknown, unknown, unknown, unknown> extends infer H
    ? { [K in keyof H]: K extends AllHOEffects ? K : never }[keyof H]
    : never;

type IntroducedFOEffectsFromHandlerHKTFor<
  HKT extends HandlerHKT,
  FOs extends AllFOEffects,
> =
  ApplyHandlerHKT<HKT, unknown, unknown> extends infer H
    ? {
        [K in keyof H]: K extends FOs
          ? ReturnType<H[K]> extends Computation<any, infer FO, any>
            ? FO
            : never
          : never;
      }[keyof H]
    : never;

type IntroducedHOEffectsFromHandlerHKTFor<
  HKT extends HandlerHKT,
  FOs extends AllFOEffects,
> =
  ApplyHandlerHKT<HKT, unknown, unknown> extends infer H
    ? {
        [K in keyof H]: K extends FOs
          ? ReturnType<H[K]> extends Computation<infer HO, any, any>
            ? HO
            : never
          : never;
      }[keyof H]
    : never;

type IntroducedHOEffectsFromHandlerReturn<HKT extends HandlerHKT> =
  ApplyHandlerHKT<HKT, unknown, unknown> extends infer H
    ? "return" extends keyof H
      ? ReturnType<H["return"]> extends Computation<infer HO, any, any>
        ? HO
        : never
      : never
    : never;

type IntroducedFOEffectsFromHandlerReturn<HKT extends HandlerHKT> =
  ApplyHandlerHKT<HKT, unknown, unknown> extends infer H
    ? "return" extends keyof H
      ? ReturnType<H["return"]> extends Computation<any, infer FO, any>
        ? FO
        : never
      : never
    : never;

type IntroducedFOEffectsFromElaboratorHKTFor<
  HKT extends ElaboratorHKT,
  SubHO extends SubHOEffects,
  SubFO extends SubFOEffects,
  HOs extends AllHOEffects,
> =
  ApplyElaboratorHKT<HKT, unknown, SubHO, SubFO, unknown> extends infer H
    ? {
        [K in keyof H]: K extends HOs
          ? ReturnType<H[K]> extends Computation<any, infer FO, any>
            ? FO
            : never
          : never;
      }[keyof H]
    : never;

type IntroducedHOEffectsFromElaboratorHKTFor<
  HKT extends ElaboratorHKT,
  HORow extends SubHOEffects,
  FORow extends SubFOEffects,
  HOs extends AllHOEffects,
> =
  ApplyElaboratorHKT<HKT, unknown, HORow, FORow, unknown> extends infer H
    ? {
        [K in keyof H]: K extends HOs
          ? ReturnType<H[K]> extends Computation<infer HO, any, any>
            ? HO
            : never
          : never;
      }[keyof H]
    : never;

type ExtractSubEntry<C, Name extends AllHOEffects> = C extends {
  readonly [SubComputationBrand]: infer SubMap;
}
  ? Name extends keyof SubMap
    ? SubMap[Name] extends SubComputationEntry
      ? SubMap[Name]
      : { subHO: SubHOEffects; subFO: SubFOEffects }
    : { subHO: SubHOEffects; subFO: SubFOEffects }
  : { subHO: SubHOEffects; subFO: SubFOEffects };

type WithHandler<
  HKT extends HandlerHKT,
  C extends AnyComputation,
> = Computation<
  | HOEffectsOf<C>
  | IntroducedHOEffectsFromHandlerHKTFor<HKT, FOEffectsOf<C>>
  | IntroducedHOEffectsFromHandlerReturn<HKT>,
  | Exclude<FOEffectsOf<C>, HandledEffectsFromHKT<HKT>>
  | IntroducedFOEffectsFromHandlerHKTFor<HKT, FOEffectsOf<C>>
  | IntroducedFOEffectsFromHandlerReturn<HKT>,
  TransformHandlerReturn<HKT, ResultOf<C>>
>;

type WithElaborator<
  HKT extends ElaboratorHKT,
  C extends AnyComputation,
> = Computation<
  | Exclude<HOEffectsOf<C>, ElaboratedEffectsFromHKT<HKT>>
  | IntroducedHOEffectsFromElaboratorHKTFor<
      HKT,
      ExtractSubEntry<C, HOEffectsOf<C>>["subHO"],
      ExtractSubEntry<C, HOEffectsOf<C>>["subFO"],
      HOEffectsOf<C>
    >,
  | FOEffectsOf<C>
  | IntroducedFOEffectsFromElaboratorHKTFor<
      HKT,
      ExtractSubEntry<C, HOEffectsOf<C>>["subHO"],
      ExtractSubEntry<C, HOEffectsOf<C>>["subFO"],
      HOEffectsOf<C>
    >,
  ResultOf<C>
>;

type ZipAll<SubHO extends SubHOEffects, SubFO extends SubFOEffects, T> = {
  readonly [K in keyof SubHO]: K extends keyof SubFO
    ? Computation<SubHO[K], SubFO[K], T>
    : never;
};

type TupleOfLength<
  N extends number,
  T,
  Acc extends readonly [T, ...T[]] = readonly [T],
> = Acc["length"] extends N ? Acc : TupleOfLength<N, T, readonly [...Acc, T]>;

type ZipN<
  N extends number,
  SubHO extends SubHOEffects,
  SubFO extends SubFOEffects,
  T,
> = MapC<SubHO, SubFO, TupleOfLength<N, never>, T>;

type MapC<SubHO extends SubHOEffects, SubFO extends SubFOEffects, Tup, T> = {
  [K in keyof Tup]: K extends keyof SubHO & keyof SubFO
    ? Computation<SubHO[K] & AllHOEffects, SubFO[K] & AllFOEffects, T>
    : never;
};

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

type UnhandledEffectError<HO extends AllHOEffects, FO extends AllFOEffects> =
  FormatUnion<FO> extends string[]
    ? FormatUnion<HO> extends string[]
      ? [HO] extends [never]
        ? [FO] extends [never]
          ? never
          : `Unhandled first-order effects: [${TupleToString<FormatUnion<FO>>}]`
        : [FO] extends [never]
          ? `Unhandled first-order effects: [${TupleToString<FormatUnion<HO>>}]`
          : `Unhandled higher-order effects: [${TupleToString<FormatUnion<HO>>}] | Unhandled first-order effects: [${TupleToString<FormatUnion<FO>>}]`
      : never
    : never;

type AllEffectsHandled<HO, FO> = HO extends never
  ? Exclude<FO, BuiltinFOEffects> extends never
    ? true
    : false
  : false;

type RuntimeResult<FO extends AllFOEffects, A> = "async" extends FO
  ? Promise<A>
  : "ndet" extends FO
    ? A[]
    : A;

declare function ret<A>(value: A): Pure<A>;

declare function op<Name extends AllFOEffects, T>(
  effect: Name,
  payload: FOPayloadOf<Name, T>,
): Computation<never, Name, FOResumeOf<Name, T>>;

declare function hOp<
  Name extends AllHOEffects,
  T,
  SubHO extends SubHOEffects,
  SubFO extends SubFOEffects,
>(
  effect: Name,
  payload: HOPayloadOf<Name, T>,
  subcomputations: ZipAll<SubHO, SubFO, T>,
): Computation<Name, never, HOResumeOf<Name, T>> & {
  readonly [SubComputationBrand]: {
    readonly [K in Name]: { subHO: SubHO; subFO: SubFO };
  };
};

declare function seq<Current, Then extends (value: ResultOf<Current>) => any>(
  current: Current,
  then: Then,
): Sequence<Current, Then>;

declare function handles<Name extends AllFOEffects>(
  effect: Name,
): (
  target: any,
  propertyKey: string,
  descriptor: TypedPropertyDescriptor<
    (
      payload: FOPayloadOf<Name, unknown>,
      resume: (value: FOResumeOf<Name, any>) => any,
    ) => AnyComputation
  >,
) => void;

declare function elaborates<Name extends AllHOEffects>(
  effect: Name,
): (
  target: any,
  propertyKey: string,
  descriptor: TypedPropertyDescriptor<
    (
      payload: HOPayloadOf<Name, any>,
      subcomputations: readonly any[],
      resume: (value: HOResumeOf<Name, any>) => any,
    ) => AnyComputation
  >,
) => void;

declare function withHandler<HKT extends HandlerHKT>(
  handler: ApplyHandlerHKT<HKT, any, any>,
): {
  run<C extends AnyComputation>(computation: C): WithHandler<HKT, C>;
};

declare function withElaborator<HKT extends ElaboratorHKT>(
  handler: ApplyElaboratorHKT<HKT, any, any, any, any>,
): {
  run<C extends AnyComputation>(computation: C): WithElaborator<HKT, C>;
};

declare function run<HO extends AllHOEffects, FO extends AllFOEffects, A>(
  computation: Computation<HO, FO, A>,
): AllEffectsHandled<HO, FO> extends true
  ? RuntimeResult<FO, A>
  : UnhandledEffectError<HO, Exclude<FO, BuiltinFOEffects>>;

declare function pipe<A>(a: A): A;
declare function pipe<A, B>(a: A, ab: (a: A) => B): B;
declare function pipe<A, B, C>(a: A, ab: (a: A) => B, bc: (b: B) => C): C;
declare function pipe<A, B, C, D>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
): D;
declare function pipe<A, B, C, D, E>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
): E;
declare function pipe<A, B, C, D, E, F>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
): F;
declare function pipe<A, B, C, D, E, F, G>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
): G;
declare function pipe<A, B, C, D, E, F, G, H>(
  a: A,
  ab: (a: A) => B,
  bc: (b: B) => C,
  cd: (c: C) => D,
  de: (d: D) => E,
  ef: (e: E) => F,
  fg: (f: F) => G,
  gh: (g: G) => H,
): H;
declare function pipe<A, B, C, D, E, F, G, H, I>(
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
declare function pipe<A, B, C, D, E, F, G, H, I, J>(
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

declare function do_<HO extends AllHOEffects, FO extends AllFOEffects, A>(
  genFn: () => DoGenerator<HO, FO, A>,
): Computation<HO, FO, A>;

export {
  ret,
  op,
  hOp,
  seq,
  handles,
  elaborates,
  withHandler,
  withElaborator,
  run,
  pipe,
  do_,
};

export type {
  FOEffect,
  HOEffect,
  SubFOEffects,
  SubHOEffects,
  AllFOEffects,
  AllHOEffects,
  AllEffects,
  SubComputationEntry,
  SubComputationMap,
  Computation,
  AnyComputation,
  Pure,
  FOComputation,
  DoGenerator,
  HOEffectsOf,
  FOEffectsOf,
  AllEffectsOf,
  ResultOf,
  Has,
  FOPayloadOf,
  FOResumeOf,
  HOPayloadOf,
  HOSubResultsOf,
  HOResumeOf,
  Sequence,
  HandlerHKT,
  ElaboratorHKT,
  WithHandler,
  WithElaborator,
  ZipAll,
  ZipN,
};
