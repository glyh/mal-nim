import strutils, tables
import definition

func rawString*(m: MalType) : string =
  if m of MalList:
    let l = MalList(m)
    result &= "("
    for i in l.items.low..l.items.high:
      result &= rawString(l.items[i])
      if i != l.items.high: result &= " "
    result &= ")"
  elif m of MalVector:
    let v = MalVector(m)
    result &= "["
    for i in v.items.low..v.items.high:
      result &= rawString(v.items[i])
      if i != v.items.high: result &= " "
    result &= "]"
  elif m of MalHashMap:
    let h = MalHashMap(m)
    result &= "{"
    var i = 0
    for k, v in h.map.pairs:
      result &= rawString(k) & " " & rawString(v)
      if i != h.map.len - 1:
        result &= ", "
      i += 1
    result &= "}"
  elif m of MalAtom:
    let a = MalAtom(m)
    case a.atomType:
      of MalInteger:
        result = $a.intValue
      of MalDouble:
        result = $a.doubleValue
      of MalSymbol:
        result = a.id
      of MalNil:
        result = "nil"
      of MalBool:
        result = $a.boolValue
      of MalString:
        result = a.strValue
      of MalKeyword:
        result = ":" & a.key
      of MalLambda, MalBuiltInLambda:
        result = "<Lambda>"
      of MalMacro:
        result = "<Macro>"
  else:
    raise newException(FieldDefect, "Unknown syntax tree")

func `$`*(m : MalType) : string =
  if m of MalList:
    let l = MalList(m)
    result &= "("
    for i in l.items.low..l.items.high:
      result &= $l.items[i]
      if i != l.items.high: result &= " "
    result &= ")"
  elif m of MalVector:
    let v = MalVector(m)
    result &= "["
    for i in v.items.low..v.items.high:
      result &= $v.items[i]
      if i != v.items.high: result &= " "
    result &= "]"
  elif m of MalHashMap:
    let h = MalHashMap(m)
    result &= "{"
    var i = 0
    for k, v in h.map.pairs:
      result &= $k & " " & $v
      if i != h.map.len - 1:
        result &= ", "
      i += 1
    result &= "}"
  elif m of MalAtom:
    let a = MalAtom(m)
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
      of MalKeyword:
        result = ":" & a.key
      of MalLambda, MalBuiltInLambda:
        result = "<Lambda>"
      of MalMacro:
        result = "<Macro>"
  else:
    raise newException(FieldDefect, "Unknown syntax tree")
