import tables, strformat
import definition, env, printer

proc eval*(envInitial: var MalEnvironment, ast: MalType) : MalType
proc evalAST(
  env: var MalEnvironment,
  ast: MalType) : MalType =
  let ast = ast
  if ast of MalList:
    var
      l = MalList(ast)
      lResult = MalList(items: @[])
    for i in l.items:
      lResult.items.add(eval(env, i))
    return lResult
  elif ast of MalVector:
    var
      v = MalVector(ast)
      vResult = MalVector(items: @[])
    for i in v.items:
      vResult.items.add(eval(env, i))
    return vResult
  elif ast of MalHashMap:
    var
      m = MalHashMap(ast)
      mResult = MalHashMap(map: initTable[MalType, MalType]())
    for k, v in m.map.pairs:
      mResult.map[k] = eval(env, v)
    return mResult
  elif ast of MalAtom:
    let asta = MalAtom(ast)
    if asta.atomType == MalSymbol:
      return env.find(asta.id)
    else: return ast

proc eval*(
  envInitial: var MalEnvironment,
  ast: MalType) : MalType =
  var env: ptr MalEnvironment = addr envInitial
  var
    ast = ast
  while true:
    if ast of MalList:
      var l = MalList(ast)
      if l.items.len == 0:
        return l
      else:
        let args = l.items[1..^1]
        if l.items[0] of MalAtom:
          let a = MalAtom(l.items[0])
          if a.atomType == MalSymbol:
            case a.id:
              of "def!":
                var a: MalAtom
                try:
                  assert args.len == 2
                  assert args[0] of MalAtom
                  a = MalAtom(args[0])
                  assert a.atomType == MalSymbol
                except:
                  raise newException(MalSyntaxError, "Syntax error while parsing `def!`")
                let val = eval(env[], args[1])
                env[].set(a.id, val)
                return val
              of "let*":
                try:
                  var envNew = MalEnvironment(outer: env[], symbols: initTable[string, MalType]())
                  env = addr envNew
                  assert args.len == 2
                  assert args[0] of MalList or args[0] of MalVector
                  if args[0] of MalList or args[0] of MalVector:
                    var its =
                      if args[0] of MalList:
                        MalList(args[0]).items
                      else:
                        MalVector(args[0]).items
                    var i = its.low
                    while i <= its.high:
                      if i == its.high:
                        raise newException(ValueError, "Odd number of args found in let bindings, ignoring...")
                      let
                        sym = its[i]
                        val = its[i+1]
                      assert sym of MalAtom
                      let a = MalAtom(sym)
                      assert a.atomType == MalSymbol
                      env[].set(a.id, eval(env[], val))
                      i += 2
                  #[
                    result = eval(env, args[1])
                    env = env.outer
                    return result
                  ]#
                  # TCO:
                  ast = args[1]
                  continue
                except AssertionDefect:
                  raise newException(MalSyntaxError, "Syntax error while parsing `let*`")
              of "do":
                # TCO
                for i in args.low..<args.high:
                  discard eval(env[], args[i])
                #return result
                ast = args[^1]
                continue
              of "if":
                try:
                  assert args.len == 3 or args.len == 2
                except:
                  raise newException(MalSyntaxError, "Syntax error while parsing `do`")
                let condition = eval(env[], args[0])
                var conditionFailed = false
                if condition of MalAtom:
                  let a = MalAtom(condition)
                  conditionFailed =
                    a.atomType == MalNil or (a.atomType == MalBool and a.boolValue == false)
                if conditionFailed:
                  if args.len == 2:
                    return MalNilAtom
                  else:
                    ast = args[2]
                    continue
                    #eval(env, args[2])
                else:
                  ast = args[1]
                  continue
                  #eval(env, args[1])
              of "fn*":
                let
                  env = env
                  def = @args
                var
                  binds = newSeq[string]()
                  allowVarargs = false
                  nextVarargs = false
                try:
                  assert def.len == 2
                  assert def[0] of MalList or def[0] of MalVector
                  let its =
                    if def[0] of MalList:
                      MalList(def[0]).items
                    else:
                      MalVector(def[0]).items
                  for i in its.low..its.high:
                    assert its[i] of MalAtom
                    let a = MalAtom(its[i])
                    assert a.atomType == MalSymbol
                    if a.id == "&":
                      nextVarargs = true
                      allowVarargs = true
                    elif nextVarargs:
                      nextVarargs = false
                      assert(i == its.high)
                      binds.add(a.id)
                    else:
                      binds.add(a.id)
                  assert nextVarargs == false
                except:
                  raise newException(
                    MalSyntaxError,
                    "Syntax error while parsing `fn*`")
                var
                  lambda =  MalAtom(atomType: MalLambda)
                  astNew : MalType
                deepCopy(astNew, def[1])
                # maybe not necessary
                lambda.f = TCOData(
                  ast: astNew,
                  params: binds,
                  env: env[], # should be dynamically set
                  fn: proc (args: varargs[MalType]) : MalType =
                    var envNew = MalEnvironment(
                      outer: env[],
                      symbols: initTable[string, MalType]())
                    envNew.doBind(binds, @args, allowVarargs)
                    eval(envNew, astNew)
                  , VarArgs: allowVarargs)
                return lambda
              else:
                #echo MalList(l).items
                #for i in
                #echo env.symbols, "!"
                #if env.outer != nil:
                  #echo env.outer.symbols, "!!"

                l = MalList(evalAST(env[], l))
                #echo l
                let maybeF = l.items[0]
                if maybeF of MalAtom:
                  let a = MalAtom(maybeF)
                  if a.atomType == MalLambda:
                      ast = a.f.ast
                      var args: seq[MalType]
                      for i in l.items[1..^1]:
                        args.add(eval(env[], i))
                      var newEnv = MalEnvironment( #bug here
                        outer: a.f.env,
                        symbols: initTable[string, MalType]())
                      env = addr newEnv
                      env[].doBind(a.f.params, args, a.f.VarArgs)
                      continue
                  elif a.atomType == MalBuiltInLambda:
                    return
                      if l.items.len > 1:
                        a.fPrimitive(l.items[1..^1])
                      else:
                        a.fPrimitive()
        raise newException(
          ValueError,
          fmt"Expecting symbol, but got ""{$(l.items[0])}""")
    else:
      return evalAST(env[], ast)
