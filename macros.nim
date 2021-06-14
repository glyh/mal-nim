import tables, sugar, logging
import definition, env, eval#, printer

var logger = newConsoleLogger()
const builtinMacros* : Table[
  string,
  (env: var MalEnvironment, args: varargs[MalType]) -> MalType] = {
  "def!": proc (env: var MalEnvironment,
                args: varargs[MalType]) : MalType {.closure.} =
    assert args.len == 2
    assert args[0] of MalAtom
    let a = MalAtom(args[0])
    assert a.atomType == MalSymbol
    #echo args[1]
    let val = evalAST(env, args[1])
    env.set(a.id, val)
    return val,
  "let*": proc(env: var MalEnvironment,
               args: varargs[MalType]) : MalType {.closure.} =
    var envNew = MalEnvironment(outer: env, symbols: initTable[string, MalType]())
    env = envNew
    assert args.len == 2
    assert args[0] of MalList or args[0] of MalVector
    if args[0] of MalList:
      let l = MalList(args[0])
      var i = l.items.low
      while i <= l.items.high:
        if i == l.items.high:
          logger.log(lvlError, "Odd number of args found in let bindings, ignoring...")
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
      let v = MalVector(args[0])
      var i = v.items.low
      while i <= v.items.high:
        if i == v.items.high:
          logger.log(lvlError, "Odd number of args found in let bindings, ignoring...")
          break
        let
          sym = v.items[i]
          val = v.items[i+1]
        assert sym of MalAtom
        let a = MalAtom(sym)
        assert a.atomType == MalSymbol
        env.set(a.id, evalAST(env, val))
        i += 2
    result = evalAST(env, args[1])
    env = env.outer,
  "do": proc(env: var MalEnvironment,
             args: varargs[MalType]) : MalType {.closure.} =
    for i in args:
      result = evalAST(env, i),
  "if": proc(env: var MalEnvironment,
             args: varargs[MalType]) : MalType {.closure.} =
    assert args.len == 3
    let condition = evalAST(env, args[0])
    var conditionFailed = false
    if condition of MalAtom:
      let a = MalAtom(condition)
      conditionFailed =
        a.atomType == MalNil or (a.atomType == MalBool and a.boolValue == false)
    if conditionFailed:
      evalAST(env, args[2])
    else:
      evalAST(env, args[1]),
  "fn*": proc(env: var MalEnvironment,
              def: varargs[MalType]) : MalType {.closure.} =
      let
        env = env
        def = @def
      assert def.len == 2
      assert def[0] of MalList
      let l = MalList(def[0])
      var binds = newSeq[string]()
      for i in l.items:
        assert i of MalAtom
        let a = MalAtom(i)
        assert a.atomType == MalSymbol
        binds.add(a.id)
      MalAtom(
        atomType: MalLambda,
        f: proc (args: varargs[MalType]) : MalType {.closure.} =
          var envNew = MalEnvironment(outer: env)
          assert binds.len == l.items.len
          for i in 0..<binds.len:
            envNew.set(binds[i], evalAST(envNew, args[i]))
          evalAST(envNew, def[1])
      )

}.toTable()
proc initMacros*(env: var MalEnvironment) =
  for k, g in builtinMacros.pairs:
    env.set(k, MalAtom(atomType: MalMacro, g: g))
