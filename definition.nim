import tables, hashes, sugar
type
  MalAtomType* = enum
    MalInteger
    MalDouble
    MalSymbol
    MalNil
    MalBool
    MalString
    MalKeyword
    MalLambda
    MalMacro
  MalType* = ref object of RootObj
  MalList* = ref object of MalType
    items*: seq[MalType]
  MalVector* = ref object of MalType
    items*: seq[MalType]
  MalHashMap* = ref object of MalType
    map*: Table[MalType, MalType]
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
      of MalKeyword:
        key*: string
      of MalLambda:
        f*: (varargs[MalType]) -> MalType
      of MalMacro:
        g*: (var MalEnvironment, varargs[MalType]) -> MalType
      of MalNil:
        discard
  MalEnvironment* = ref object
    outer*: MalEnvironment
    symbols*: Table[string, MalType]
  MalNothingToRead* = object of CatchableError
let MalNilAtom* = MalAtom(atomType: MalNil)

func hash*(x: MalType) : Hash =
  var h: Hash = 0
  if x of MalList:
    h = h !& hash("MalList")
    for i in MalList(x).items:
      h = h !& hash(i)
  elif x of MalVector:
    h = h !& hash("MalVector")
    for i in MalVector(x).items:
      h = h !& hash(i)
  elif x of MalHashMap:
    h = h !& hash("MalHashMap")
    for k, v in MalHashMap(x).map.pairs:
      h = h !& hash(k) !& hash(v)
  elif x of MalAtom:
    h = h !& hash("MalAtom")
    let a = MalAtom(x)
    case a.atomType:
      of MalInteger:
        h = h !& hash("int") !& hash(a.intValue)
      of MalDouble:
        h = h !& hash("double") !& hash(a.doubleValue)
      of MalSymbol:
        h = h !& hash("symbol") !& hash(a.id)
      of MalString:
        h = h !& hash("string") !& hash(a.strValue)
      of MalBool:
        h = h !& hash("bool") !& hash(a.boolValue)
      of MalKeyword:
        h = h !& hash("keyword") !& hash(a.key)
      of MalNil:
        h = h !& hash("nil")
      of MalLambda:
        raise newException(FieldDefect, "Trying to hash lambdas!")
      of MalMacro:
        raise newException(FieldDefect, "Trying to hash macros!")
  result = !$h
