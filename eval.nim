import tables, strformat
import definition, env, printer

var outerMost*: MalEnvironment

proc isMacroCall(ast: MalType, env: MalEnvironment): bool =
  try:
    return ast of MalList and (
      let l = MalList(ast)
      l.items.len >= 1 and (
        let firstSym = l.items[0]
        firstSym of MalAtom and (
          let a = MalAtom(firstSym)
          a.atomType == MalSymbol and (
            let lookedUp = env.find(a.id)
            lookedUp of MalAtom and (
              let al = MalAtom(lookedUp)
              al.atomType == MalLambda and al.f.isMacro)))))
  except:
    return false

proc macroExpand(ast: MalType, env: MalEnvironment): MalType =
  var ast = ast
  while isMacroCall(ast, env):
    let m : TCOData = MalAtom(env.find(MalAtom(MalList(ast).items[0]).id)).f
    if MalList(ast).items.len > 1:
      #echo MalList(ast).items
      ast = m.fn(MalList(ast).items[1..^1])
    else:
      ast = m.fn()
  ast

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

var isDefMacro = false
proc eval*(
  envInitial: var MalEnvironment,
  ast: MalType) : MalType =

  proc quasiquote(args : varargs[MalType]) : MalType =
    try:
      assert args.len == 1
      let arg = args[0]
      if arg of MalList:
        let l = MalList(arg)
        if l.items.len == 2 and
           l.items[0] of MalAtom and
           MalAtom(l.items[0]).atomType == MalSymbol and
           MalAtom(l.items[0]).id == "unquote":
          #if l.items[1] of MalAtom and
          #  MalAtom(l.items[1]).atomType == MalSymbol:
            return l.items[1]
          #else:
          #  return MalList(items:
          #    @[MalAtom(atomType: MalSymbol, id: "quote"), l])
        else:
          result = MalList(items: @[])
          if l.items.len == 0:
            return result
          for id in countdown(l.items.high, l.items.low):
            let elt = l.items[id]
            if elt of MalList:
              let eltl = MalList(elt)
              if eltl.items.len == 2 and
                 eltl.items[0] of MalAtom and
                 MalAtom(eltl.items[0]).atomType == MalSymbol and
                 MalAtom(eltl.items[0]).id == "splice-unquote":
                result = MalList(items:
                  @[MalAtom(atomType: MalSymbol, id: "concat"),
                    eltl.items[1],
                    result])
              else:
                result = MalList(items:
                  @[MalAtom(atomType: MalSymbol, id: "cons"),
                    quasiquote(elt),
                    result])
            else:
              result = MalList(items:
                @[MalAtom(atomType: MalSymbol, id: "cons"),
                  quasiquote(elt),
                  result])
          return result
      elif arg of MalAtom:
        if MalAtom(arg).atomType == MalSymbol:
          return MalList(items:
            @[MalAtom(atomType: MalSymbol, id: "quote"),
              arg])
      elif arg of MalHashMap:
        return MalList(items:
          @[MalAtom(atomType: MalSymbol, id: "quote"),
            arg])
      elif arg of MalVector:
        return MalList(items:
          @[MalAtom(atomType: MalSymbol, id: "vec") ,
          quasiquote(MalList(items: MalVector(arg).items))])
      arg
    except:
      raise newException(
        FieldDefect,
        "Wrong parameters passed to function `quasiquote`")

  var
    env: ptr MalEnvironment = addr envInitial
    ast = macroExpand(ast, env[])
  echo "expandededede! ", ast

  while true:
    if ast of MalList:
      var l = MalList(ast)
      #echo MalList(l).items
      if l.items.len == 0:
        return l
      else:
        let
          args = l.items[1..^1]
          head =
            if l.items[0] of MalAtom:
              l.items[0]
            else:
              eval(env[], l.items[0])
        if head of MalAtom:
          let a = MalAtom(head)
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
              of "defmacro!":
                isDefMacro = true
                var a: MalAtom
                try:
                  assert args.len == 2
                  assert args[0] of MalAtom
                  a = MalAtom(args[0])
                  assert a.atomType == MalSymbol
                except:
                  isDefMacro = false
                  raise newException(MalSyntaxError, "Syntax error while parsing `def!`")
                let val = eval(env[], args[1])
                env[].set(a.id, val)
                isDefMacro = false
                return val
              of "macroexpand":
                try:
                  assert args.len == 1
                  return macroExpand(args[0], env[])
                except AssertionDefect:
                  raise newException(MalSyntaxError, "Syntax error while parsing `let*`")
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
                  ast = args[1]
                  continue
                except AssertionDefect:
                  raise newException(MalSyntaxError, "Syntax error while parsing `let*`")
              of "do":
                for i in args.low..<args.high:
                  discard eval(env[], args[i])
                ast = args[^1]
                continue
              of "if":
                echo "if<- ", args
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
                else:
                  ast = args[1]
                  continue
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
                  env: env[],
                  fn: proc (args: varargs[MalType]) : MalType =
                    var envNew = MalEnvironment(
                      outer: env[],
                      symbols: initTable[string, MalType]())
                    #echo "wowooowwowo!"
                    #echo args
                    envNew.doBind(binds, @args, allowVarargs)
                    #echo args
                    eval(envNew, astNew)
                  , VarArgs: allowVarargs,
                  isMacro: isDefMacro)
                return lambda
              of "eval":
                try: #Evaluate twice
                  assert args.len == 1
                  ast = eval(env[], args[0])
                  env = addr outerMost
                  continue
                except:
                  raise newException(MalSyntaxError, "Syntax error while parsing `eval`")
              of "quote":
                if args.len != 1:
                  raise newException(MalSyntaxError, "Syntax error while parsing `quote`")
                return args[0]
              of "quasiquoteexpand":
                try:
                  assert args.len == 1
                  return quasiquote(args[0])
                except:
                  raise newException(MalSyntaxError, "Syntax error while parsing `quasiquote`")
              of "quasiquote":
                try:
                  assert args.len == 1
                  ast = quasiquote(args[0])
                  continue
                except:
                  raise newException(MalSyntaxError, "Syntax error while parsing `quasiquote`")
              else:
                l = MalList(evalAST(env[], l))
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
          elif a.atomType == MalLambda:
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
