import logging, strutils
import definition
var logger = newConsoleLogger()
proc `$`*(v : MalType) : string =
  if v of MalList:
    let l = MalList(v)
    result &= "("
    for i in l.items.low..l.items.high:
      result &= $l.items[i]
      if i != l.items.high: result &= " "
    result &= ")"
  elif v of MalAtom:
    let a = MalAtom(v)
    case a.atomType:
      of MalInteger:
        result = $a.intValue
      of MalDouble:
        result = $a.doubleValue
      of MalSymbol:
        result = $a.id
      of MalNil:
        result = "nil"
      of MalBool:
        result = $a.boolValue
      of MalString:
        result = escape(a.strValue)
  else:
    logger.log(lvlError, "Unknown syntax tree")
