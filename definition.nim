type
  MalAtomType* = enum
    MalInteger
    MalDouble
    MalSymbol
    MalNil
    MalBool
    MalString
  MalType* = ref object of RootObj
  MalList* = ref object of MalType
    items*: seq[MalType]
  MalAtom* = ref object of MalType
    case atomType*: MalAtomType
      of MalInteger:
        intValue*: int64
      of MalDouble:
        doubleValue*: float64
      of MalSymbol:
        id*: string
      of MalString:
        strValue*: string
      of MalBool:
        boolValue*: bool
      of MalNil:
        discard
