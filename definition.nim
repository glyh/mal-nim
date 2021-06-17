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
    MalBuiltInLambda
    MalMacro
    MalAtomValue
  MalType* = ref object of RootObj
  MalList* = ref object of MalType
    items*: seq[MalType]
  MalVector* = ref object of MalType
    items*: seq[MalType]
  MalHashMap* = ref object of MalType
    map*: Table[MalType, MalType]
  TCOData* = object
    ast*: MalType
    params*: seq[string]
    env*: MalEnvironment
    #fn*: MalType
    fn*: (varargs[MalType]) -> MalType
    VarArgs*: bool
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
        f*: TCOData
      of MalBuiltInLambda:
        fPrimitive*: (varargs[MalType]) -> MalType
      of MalMacro:
        g*: (var MalEnvironment, varargs[MalType]) -> MalType
      of MalNil:
        discard
      of MalAtomValue:
        p*: MalType
  MalEnvironment* = ref object
    outer*: MalEnvironment
    symbols*: Table[string, MalType]
  MalNothingToRead* = object of CatchableError
  MalReadRBracket* = object of CatchableError
  MalSyntaxError* = object of CatchableError
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
      of MalLambda, MalBuiltInLambda:
        raise newException(FieldDefect, "Trying to hash lambdas!")
      of MalMacro:
        raise newException(FieldDefect, "Trying to hash macros!")
      of MalAtomValue:
        raise newException(FieldDefect, "Trying to hash atoms!")
  result = !$h

#[
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
]#
proc `==`*(lhs: MalType, rhs: MalType) : bool =
  if (lhs of MalList or lhs of MalVector) and (rhs of MalList or rhs of MalVector):
    let
      il =
        if lhs of MalList:
          MalList(lhs).items
        else:
          MalVector(lhs).items
      ir =
        if rhs of MalList:
          MalList(rhs).items
        else:
          MalVector(rhs).items
    il == ir
  elif lhs of MalHashMap and rhs of MalHashMap:
    let
      ml = MalHashMap(lhs)
      mr = MalHashMap(rhs)
    ml.map == mr.map
  elif lhs of MalAtom and rhs of MalAtom:
    let
      al = MalAtom(lhs)
      ar = MalAtom(rhs)
    return al.atomType == ar.atomType and (
        case al.atomType:
          of MalInteger:
            al.intValue == ar.intValue
          of MalDouble:
            al.doubleValue == ar.doubleValue
          of MalSymbol:
            al.id == ar.id
          of MalNil:
            true
          of MalBool:
            al.boolValue == ar.boolValue
          of MalString:
            al.strValue == ar.strValue
          of MalKeyword:
            al.key == ar.key
          of MalLambda, MalBuiltInLambda:
            raise newException(FieldDefect, "Trying to compare lambdas!")
          of MalMacro:
            raise newException(FieldDefect, "Trying to compare macros!")
          of MalAtomValue:
            raise newException(FieldDefect, "Trying to compare atoms!")
      )
  else:
    false
