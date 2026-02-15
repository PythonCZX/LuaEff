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
} from "./LuaEff";

const Test = new Handler()
  .handle(IO, (payload, resume) => op(FAIL, undefined))
  .handle(FAIL, (payload, resume) => op(IO, () => print("H")))
  .build();
