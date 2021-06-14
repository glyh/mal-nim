import logging, tables, sugar, strformat, sequtils
import definition, eval

var logger = newConsoleLogger()

proc set*(env: var MalEnvironment, symbol: string, value: MalType) =
  env.symbols[symbol] = value

proc get*(env: MalEnvironment, symbol: string) : MalEnvironment =
  var e = env
  while e.outer != nil and not e.symbols.hasKey(symbol): e = e.outer
  return e

proc find*(env: MalEnvironment, symbol: string) : MalType =
  var e = get(env, symbol)
  if e.symbols.hasKey(symbol): e.symbols[symbol]
  else:
    logger.log(lvlError, fmt"The symbol ""{symbol}"" doesn't exist in this context")
    MalNilAtom


const builtinFunctions* : Table[string, (varargs[MalType]) -> MalType] = {
  "+": proc (args: varargs[MalType]) : MalType {.closure.} =
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
      logger.log(lvlError, "Type error in function `+`")
      return MalNilAtom,
  "-": proc (args: varargs[MalType]) : MalType {.closure.} =
    try:
      case args.len:
        of 0:
          logger.log(lvlError, "Wrong number of parameters in function `-`")
          return MalNilAtom
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
      logger.log(lvlError, "Type error in function `-`")
      return MalAtom(atomType: MalInteger, intValue: 0),
  "*": proc (args: varargs[MalType]): MalType {.closure.} =
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
      logger.log(lvlError, "Type error in function `*`")
      return MalAtom(atomType: MalInteger, intValue: 0),
  "/": proc (args: varargs[MalType]): MalType {.closure.} =
    try:
      case args.len:
        of 0:
          logger.log(lvlError, "Wrong number of parameters in function `-`")
          return MalAtom(atomType: MalInteger, intValue: 0)
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
      logger.log(lvlError, "Type error in function `/`!")
      return MalAtom(atomType: MalInteger, intValue: 0)
    except DivByZeroDefect:
      logger.log(lvlError, "Divded by zero in function `/`!")
      return MalAtom(atomType: MalInteger, intValue: 0),
  "quit": proc (args: varargs[MalType]) : MalType {.closure.} =
    echo "Goodbye!"
    quit()
}.toTable()

var defaultEnvironment* = MalEnvironment(
  symbols: initTable[string, MalType](),
  outer: nil)
for k, f in builtinFunctions.pairs:
  defaultEnvironment.set(k, MalAtom(atomType: MalLambda, f: f))
