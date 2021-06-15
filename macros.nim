#[
  import tables, sugar, logging
  import definition, env, eval, printer

  var logger = newConsoleLogger()
  const builtinMacros* : Table[
    string,
    (env: var MalEnvironment, args: varargs[MalType]) -> MalType] = {
    "def!": proc (env: var MalEnvironment,
                  args: varargs[MalType]) : MalType {.closure.} =
                    var a: MalAtom
      try:
        assert args.len == 2
        assert args[0] of MalAtom
        a = MalAtom(args[0])
        assert a.atomType == MalSymbol
      except:
        raise newException(MalSyntaxError, "Syntax error while parsing `def!`")
      let val = eval(env, args[1])
      env.set(a.id, val)
      return val
    , "let*": proc(env: var MalEnvironment,
                 args: varargs[MalType]) : MalType {.closure.} =
      try:
        var envNew = MalEnvironment(outer: env, symbols: initTable[string, MalType]())
        env = envNew
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
              logger.log(lvlError, "Odd number of args found in let bindings, ignoring...")
              break
            let
              sym = its[i]
              val = its[i+1]
            assert sym of MalAtom
            let a = MalAtom(sym)
            assert a.atomType == MalSymbol
            env.set(a.id, eval(env, val))
            i += 2
        result = eval(env, args[1])
        env = env.outer
      except:
        raise newException(MalSyntaxError, "Syntax error while parsing `let*`")
    , "do": proc(env: var MalEnvironment,
               args: varargs[MalType]) : MalType {.closure.} =
      for i in args.items:
        result = eval(env, i)
    , "if": proc(env: var MalEnvironment,
               args: varargs[MalType]) : MalType {.closure.} =
      try:
        assert args.len == 3 or args.len == 2
      except:
        raise newException(MalSyntaxError, "Syntax error while parsing `do`")
      let condition = eval(env, args[0])
      var conditionFailed = false
      if condition of MalAtom:
        let a = MalAtom(condition)
        conditionFailed =
          a.atomType == MalNil or (a.atomType == MalBool and a.boolValue == false)
      if conditionFailed:
        if args.len == 2:
          MalNilAtom
        else:
          eval(env, args[2])
        else:
          eval(env, args[1])
    , "fn*": proc(env: var MalEnvironment,
                def: varargs[MalType]) : MalType {.closure.} =
      try:
        let
          env = env
          def = @def
        assert def.len == 2
        assert def[0] of MalList or def[0] of MalVector
        let its =
          if def[0] of MalList:
            MalList(def[0]).items
          else:
            MalVector(def[0]).items
        var
          binds = newSeq[string]()
          allowVarargs = false
          nextVarargs = false
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
        MalAtom(
          atomType: MalLambda,
          f: proc (args: varargs[MalType]) : MalType {.closure.} =
            var
              envTmp = MalEnvironment(outer: env)
              envNew = MalEnvironment(outer: env)
            assert binds.len == args.len or
              (binds.len <= args.len + 1 and allowVarargs)
            for i in 0..binds.high:
              if i == binds.len - 1 and allowVarargs:
                var js = newSeq[MalType]()
                for j in i..args.high:
                  js.add(eval(envTmp, args[j]))
                envNew.set(binds[i], MalList(items: js))
              else:
                envNew.set(binds[i], eval(envTmp, args[i]))
            var defNew : MalType
            deepCopy(defNew, def[1])
            eval(envNew, defNew)
        )
      except:
        raise newException(MalSyntaxError, "Syntax error while parsing `fn*`")
  }.toTable()
  proc initMacros*(env: var MalEnvironment) =
    for k, g in builtinMacros.pairs:
      env.set(k, MalAtom(atomType: MalMacro, g: g))
]#
