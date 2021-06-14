import tables, logging
import definition
var logger = newConsoleLogger()

var envrinoment* = {
  "+": proc (params: varargs[MalType]): MalType =
    try:
      var
        ansInt: int64 = 0
        ansDouble: float64 = 0
        isDouble = false
      for i in params:
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
        return MalAtom( atomType: MalDouble, doubleValue: ansDouble)
      else:
        return MalAtom(atomType: MalInteger, intValue: ansInt)
    except AssertionDefect:
      logger.log(lvlError, "Type error in function `+`")
      return MalAtom(atomType: MalInteger, intValue: 0),
  "-": proc (params: varargs[MalType]): MalType =
    try:
      case params.len:
        of 0:
          logger.log(lvlError, "Wrong number of parameters in function `-`")
          return MalAtom(atomType: MalInteger, intValue: 0)
        of 1:
          let x = params[0]
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
            for i in params:
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
  "*": proc (params: varargs[MalType]): MalType =
    try:
      var
        ansInt: int64 = 1
        ansDouble: float64 = 1
        isDouble = false
      for i in params:
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
  "/": proc (params: varargs[MalType]): MalType =
    try:
      case params.len:
        of 0:
          logger.log(lvlError, "Wrong number of parameters in function `-`")
          return MalAtom(atomType: MalInteger, intValue: 0)
        of 1:
          let x = params[0]
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
            for i in params:
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
      return MalAtom(atomType: MalInteger, intValue: 0)
}.toTable()

proc evalAST(ast: MalType) : MalType =
  assert false # To be implemented
  if ast of MalList:
    var l = MalList(ast)
    if l.items.len >= 1:
      l.items[0] = evalAST(l.items[0])
      #and l.items[0] of MalAtom

    for i in l.items.mitems:
      i = evalAST(i)
    l
  elif ast of MalVector:
    let v = MalVector(ast)
    for i in v.items.mitems:
      i = evalAST(i)
    v
  elif ast of MalHashMap:
    let m = MalHashMap(ast)
    for k, v in m.map.mpairs:
      k = evalAST(k)
      v = evalAST(v)
    m
  elif ast of MalAtom:
    let a = MalAtom(ast)
    case a.atomType:
      of Symbol
