import logging, tables, sugar, strformat
import definition, env, printer

var builtinFunctions* :
  Table[string, proc(args: varargs[MalType]) : MalType {.closure.} ]

builtinFunctions["+"] =
  proc (args: varargs[MalType]) : MalType {.closure.} =
    try:
      var
        ansInt: int64 = 0
        ansDouble: float64 = 0
        isDouble = false
      for i in args:
        assert(i of MalAtom)
        let ia = MalAtom(i)
        assert(ia.atomType in {MalInteger, MalDouble})
        if ia.atomType == MalDouble:
          isDouble = true
          ansDouble += ia.doubleValue
        else:
          ansDouble += float64(ia.intValue)
          ansInt += ia.intValue
      if isDouble:
        return MalAtom(atomType: MalDouble, doubleValue: ansDouble)
      else:
        return MalAtom(atomType: MalInteger, intValue: ansInt)
    except AssertionDefect:
      raise newException(FieldDefect, "Type error in function `+`")
builtinFunctions["-"] =
  proc (args: varargs[MalType]) : MalType {.closure.} =
    try:
      case args.len:
        of 0:
          raise newException(FieldDefect, "Wrong number of parameters passed to function `-`")
        of 1:
          let x = args[0]
          assert (x of MalAtom)
          let xa = MalAtom(x)
          assert (xa.atomType in {MalInteger, MalDouble})
          if xa.atomType == MalInteger:
            return MalAtom( atomType: MalInteger, intValue: -xa.intValue)
          else:
            return MalAtom( atomType: MalDouble, doubleValue: -xa.doubleValue)
        else:
          var
            ansInt: int64 = 0
            ansDouble: float64 = 0
            isDouble = false
            sign = 1
          for i in args:
            assert(i of MalAtom)
            let ia = MalAtom(i)
            assert(ia.atomType in {MalInteger, MalDouble})
            if ia.atomType == MalDouble:
              isDouble = true
              ansDouble += float64(sign) * ia.doubleValue
            else:
              ansDouble += float64(sign * ia.intValue)
              ansInt += sign * ia.intValue
            if sign == 1: sign = -1
          if isDouble:
            return MalAtom( atomType: MalDouble, doubleValue: ansDouble)
          else:
            return MalAtom(atomType: MalInteger, intValue: ansInt)
    except AssertionDefect:
      raise newException(FieldDefect, "Type error in function `-`")

builtinFunctions["*"] =
  proc (args: varargs[MalType]): MalType  =
    try:
      var
        ansInt: int64 = 1
        ansDouble: float64 = 1
        isDouble = false
      for i in args:
        assert(i of MalAtom)
        let ia = MalAtom(i)
        assert(ia.atomType in {MalInteger, MalDouble})
        if ia.atomType == MalDouble:
          isDouble = true
          ansDouble *= ia.doubleValue
        else:
          ansDouble *= float64(ia.intValue)
          ansInt *= ia.intValue
      if isDouble:
        return MalAtom( atomType: MalDouble, doubleValue: ansDouble)
      else:
        return MalAtom(atomType: MalInteger, intValue: ansInt)
    except AssertionDefect:
      raise newException(FieldDefect, "Type error in function `*`")

builtinFunctions["/"] =
  proc (args: varargs[MalType]): MalType  =
    try:
      case args.len:
        of 0:
          raise newException(FieldDefect, "Wrong number of parameters passed to function `/`")
        of 1:
          let x = args[0]
          assert (x of MalAtom)
          let xa = MalAtom(x)
          assert (xa.atomType in {MalInteger, MalDouble})
          if xa.atomType == MalInteger:
            return MalAtom(
              atomType: MalDouble, doubleValue: 1.0 / float64(xa.intValue))
          else:
            return MalAtom(
              atomType: MalDouble, doubleValue: 1.0 / xa.doubleValue)
        else:
          var
            ansInt: int64 = 1
            ansDouble: float64 = 1
            isDouble = false
            mul = true
          for i in args:
            assert(i of MalAtom)
            let ia = MalAtom(i)
            assert(ia.atomType in {MalInteger, MalDouble})
            if ia.atomType == MalDouble:
              isDouble = true
              if mul:
                ansDouble *= ia.doubleValue
                mul = false
              else:
                ansDouble /= ia.doubleValue
            else:
              if mul:
                ansDouble *= float64(ia.intValue)
                ansInt *= ia.intValue
                mul = false
              else:
                ansDouble /= float64(ia.intValue)
                ansInt = ansInt div ia.intValue
          if isDouble:
            return MalAtom( atomType: MalDouble, doubleValue: ansDouble)
          else:
            return MalAtom(atomType: MalInteger, intValue: ansInt)
    except AssertionDefect:
      raise newException(FieldDefect, "Type error in function `/`")
    except DivByZeroDefect:
      raise newException(DivByZeroDefect, "Type by zero in function `/`")

builtinFunctions["quit"] =
  proc (args: varargs[MalType]): MalType {.closure.} =
    raise newException(EOFError, "")

builtinFunctions["list"] =
  proc (args: varargs[MalType]) : MalType {.closure.} =
    MalList(items: @args)

builtinFunctions["list?"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    if args.len != 1:
      raise newException(FieldDefect, "Wrong number of parameters passed to function `list?`")
    MalAtom(atomType: MalBool, boolValue: args[0] of MalList)

builtinFunctions["empty?"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    if args.len != 1:
      raise newException(FieldDefect, "Wrong number of parameters passed to function `empty?`")
    if args[0] of MalList:
      return MalAtom(atomType: MalBool,
                     boolValue: MalList(args[0]).items.len == 0)
    elif args[0] of MalVector:
      return MalAtom(atomType: MalBool,
                     boolValue: MalVector(args[0]).items.len == 0)
    elif args[0] of MalHashMap:
      return MalAtom(atomType: MalBool,
                     boolValue: MalHashMap(args[0]).map.len == 0)
    else:
      raise newException(
        FieldDefect,
        fmt"Input invalid value ""{args[0]}"" in function `empty?`")

builtinFunctions["count"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    if args.len != 1:
      raise newException(FieldDefect, "Wrong number of parameters passed to function `count`")
    if args[0] of MalList:
      return MalAtom(atomType: MalInteger,
                     intValue: MalList(args[0]).items.len)
    elif args[0] of MalVector:
      return MalAtom(atomType: MalInteger,
                     intValue: MalVector(args[0]).items.len)
    elif args[0] of MalHashMap:
      return MalAtom(atomType: MalInteger,
                     intValue: MalHashMap(args[0]).map.len)
    else:
      raise newException(
        FieldDefect,
        fmt"Input invalid value ""{args[0]}"" in function `count`")

builtinFunctions["="] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    var ans = true
    if args.len > 1:
      for i in args.low..args.high-1:
        ans = ans and (args[i] == args[i+1])
        if not ans: break
    MalAtom(atomType: MalBool, boolValue: ans)

builtinFunctions[">"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    try:
      for i in args:
        assert i of MalAtom
        let a = MalAtom(i)
        assert a.atomType in {MalInteger, MalDouble}
    except Exception:
      raise newException(FieldDefect, fmt"Input invalid value in function `>`")
    var ans = true
    if args.len > 1:
      var last: float64 = 0
      for i in args.low..args.high:
        let
          a = MalAtom(args[i])
          cur =
            if a.atomType == MalInteger:
              float64(a.intValue)
            else:
              a.doubleValue
        if i != args.low:
          ans = ans and (last > cur)
        if not ans: break
        last = cur
    MalAtom(atomType: MalBool, boolValue: ans)

builtinFunctions["<"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    try:
      for i in args:
        assert i of MalAtom
        let a = MalAtom(i)
        assert a.atomType in {MalInteger, MalDouble}
    except Exception:
      raise newException(FieldDefect, fmt"Input invalid value in function `<`")
    var ans = true
    if args.len > 1:
      var last: float64 = 0
      for i in args.low..args.high:
        let
          a = MalAtom(args[i])
          cur =
            if a.atomType == MalInteger:
              float64(a.intValue)
            else:
              a.doubleValue
        if i != args.low:
          ans = ans and (last < cur)
        if not ans: break
        last = cur
    MalAtom(atomType: MalBool, boolValue: ans)

builtinFunctions["pr-str"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    var s = ""
    for i in args:
      if s != "":
        s &= " " & $i
      else:
        s = $i
    MalAtom(atomType: MalString, strValue: s)

builtinFunctions["str"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    var s = ""
    for i in args:
      if s != "":
        s &= " " & rawString(i)
      else:
        s = rawString(i)
    MalAtom(atomType: MalString, strValue: s)

builtinFunctions["prn"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    var s = ""
    for i in args:
      if s != "":
        s &= " " & $i
      else:
        s = $i
    echo s
    MalAtom(atomType: MalNil)

builtinFunctions["println"] =
  proc(args: varargs[MalType]) : MalType {.closure.} =
    var s = ""
    for i in args:
      if s != "":
        s &= " " & rawString(i)
      else:
        s = rawString(i)
    echo s
    MalAtom(atomType: MalNil)

var defaultEnvironment* = MalEnvironment(
  symbols: initTable[string, MalType](),
  outer: nil)
for k, f in builtinFunctions.pairs:
  defaultEnvironment.set(k, MalAtom(atomType: MalBuiltInLambda, fPrimitive: f))
