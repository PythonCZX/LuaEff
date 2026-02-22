import {
  run,
  Computation,
  seq,
  ret,
  AnyComputation,
  op,
  Has,
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
  perform,
  WithHandler,
  HandledEffectsFromHKT,
  IntroducedFOEffectsFromHandlerHKTFor,
  IntroducedFOEffectsFromHandlerReturn,
} from "./LuaEff";

declare module "./LuaEff" {
  interface FOEffects<T> {
    get: FOEffect<void, T>;
    set: FOEffect<T, void>;
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

type TTEES = IntroducedFOEffectsFromHandlerHKTFor<h, FOEffectsOf<p>>;

function runState<S>(initial: S) {
  return withHandler<StateHKT<S>>(new StateHandler(initial)).run;
}

const program = do_(function* () {
  const a = yield* perform(get());
  const b = 2 * a;
  yield set(b);
  return "Done.";
});

const handled = run(runState(13)(program));
