import tables, sugar, logging
import definition, env, eval#, printer

var logger = newConsoleLogger()
const builtinMacros* : Table[
  string,
  (env: var MalEnvironment, params: varargs[MalType]) -> MalType] = {
  "def!": proc (env: var MalEnvironment,
                params: varargs[MalType]) : MalType {.closure.} =
    assert params.len == 2
    assert params[0] of MalAtom
    let a = MalAtom(params[0])
    assert a.atomType == MalSymbol
    #echo params[1]
    let val = evalAST(env, params[1])
    env.set(a.id, val)
    return val,
  "let*": proc(env: var MalEnvironment,
               params: varargs[MalType]) : MalType {.closure.} =
    var envNew = MalEnvironment(outer: env, symbols: initTable[string, MalType]())
    env = envNew
    assert params.len >= 2
    assert params[0] of MalList or params[0] of MalVector
    if params[0] of MalList:
      let l = MalList(params[0])
      var i = l.items.low
      while i <= l.items.high:
        if i == l.items.high:
          logger.log(lvlError, "Odd number of params found in let bindings, ignoring...")
          break
        let
          sym = l.items[i]
          val = l.items[i+1]
        assert sym of MalAtom
        let a = MalAtom(sym)
        assert a.atomType == MalSymbol
        env.set(a.id, evalAST(env, val))
        i += 2
    else:
      let v = MalVector(params[0])
      var i = v.items.low
      while i <= v.items.high:
        if i == v.items.high:
          logger.log(lvlError, "Odd number of params found in let bindings, ignoring...")
          break
        let
          sym = v.items[i]
          val = v.items[i+1]
        assert sym of MalAtom
        let a = MalAtom(sym)
        assert a.atomType == MalSymbol
        env.set(a.id, evalAST(env, val))
        i += 2
    for i in 1..params.high:
      result = evalAST(env, params[i])
    env = env.outer
}.toTable()
proc initMacros*(env: var MalEnvironment) =
  for k, g in builtinMacros.pairs:
    env.set(k, MalAtom(atomType: MalMacro, g: g))
