import {
  ASYNC,
  Handler,
  IO,
  op,
  ret,
  FAIL,
  AllFOEffects,
  FOEffects,
  AnyComputation,
  ApplyReturnTransform,
  Replaced,
  _,
  Pure,
  seq,
  Computation,
} from "./LuaEff";

const comp = op<"io", void>(IO, () => print("Msg"));

type Test = ApplyReturnTransform<<A>(x: A) => Array<A>, string>;
type PlaceHolder = "a";
type SomeComp = (x: _) => {
  hello: _;
  by: _[];
  wow: (x: string, y: string) => _;
};
type DoMappings<F, A> = F extends undefined
  ? A
  : F extends (x: _) => infer R
    ? Replaced<R, _, A>
    : A;

type Gra = SomeComp extends (x: _) => infer R ? R : never;
type O = Replaced<Gra, _, string>;

const sequencing = seq(ret(42), v => ret(v));
type TYY = Replaced<Computation<never, "io", _[]>, _, string>;
type Testo = DoMappings<SomeComp, string>;

const Test = new Handler()
  .handle(IO, (payload, resume) => ret(1))
  .handle(FAIL, (payload, resume) => op(IO, () => print("H")))
  .mapReturn((x: _) =>
    seq(
      op<"io", void>(IO, () => print("Returning")),
      () => ret([x]),
    ),
  )
  // .mapReturn((x: _) => ret([x]))
  .build()
  .run(comp);
