import re, logging, parseutils, strutils, system, tables, strformat
import definition
var logger = newConsoleLogger()

type
  Reader = object
    tokens : seq[string]

func peak(r: Reader) : string =
  r.tokens[0]

func empty(r : Reader) : bool =
  r.tokens.len == 0

proc forward(r: var Reader) =
  r.tokens.delete(0)

proc readForm(r: var Reader) : MalType

proc readList(r: var Reader) : MalList =
  r.forward()
  result = MalList(items: @[])
  while not r.empty():
    if r.peak() == ")":
      r.forward()
      return result
    else:
      try:
        result.items.add(r.readForm())
      except MalReadRBracket:
        let rb = getCurrentExceptionMsg()
        if rb != ")":
          raise newException(ValueError, fmt"Expecting "")"", got {rb}")
        else:
          break
  raise newException(ValueError, "Missing right parenthesis!")

proc readVector(r : var Reader) : MalVector =
  r.forward()
  result = MalVector(items: @[])
  while not r.empty():
    if r.peak() == "]":
      r.forward()
      return result
    else:
      try:
        result.items.add(r.readForm())
      except MalReadRBracket:
        let rb = getCurrentExceptionMsg()
        if rb != "]":
          raise newException(ValueError, fmt"Expecting ""]"", got {rb}")
        else:
          break
  raise newException(ValueError, "Missing right parenthesis!")

proc readHashMap(r : var Reader) : MalHashMap =
  r.forward()
  result = MalHashMap(map: initTable[MalType, MalType]())
  while not r.empty():
    if r.peak() == "}":
      r.forward()
      return result
    else:
      try:
        let k = r.readForm()
        if r.empty():
          logger.log(lvlError, "Number of elements in map is incorrect!")
          return result
        let v = r.readForm()
        if result.map.hasKey(k):
          logger.log(lvlWarn, "Duplicate key ignored.")
        else:
          result.map[k] = v
      except MalReadRBracket:
        let rb = getCurrentExceptionMsg()
        if rb != "]":
          raise newException(ValueError, fmt"Expecting ""}}"", got {rb}")
        else:
          break
  raise newException(ValueError, "Missing right parenthesis!")

proc readAtom(r: var Reader) : MalAtom =
  let cur = r.peak()
  if match(cur, re"^[+-]?[0-9]+$"):
    var i: int64
    discard parseBiggestInt(cur, i)
    result = MalAtom(atomType: MalInteger, intValue: i)
  elif match(cur, re"^[+-]?\d*\.[0-9]+(e[+-]?\d+)?$"):
    var f: float64
    discard parseFloat(cur, f)
    result = MalAtom(atomType: MalDouble, doubleValue: f)
  elif match(cur, re"^""(\\.|[^""\\])*""?$"):
    if cur[^1] != '"':
      raise newException(ValueError, "Missing double quote for string!")
    else:
      var strReal = cur.unescape().replace("\\n", "\n")
      result = MalAtom(atomType: MalString, strValue: strReal)
  elif match(cur, re("^(true|false)$")):
    result =  MalAtom(atomType: MalBool, boolValue: cur == "true")
  elif match(cur, re("^nil$")):
    result = MalAtom(atomType: MalNil)
  elif match(cur, re("^:.*$")):
    result = MalAtom(atomType: MalKeyword, key: cur[1..^1])
  else:
    result = MalAtom(atomType: MalSymbol, id: cur)
  r.forward()

proc readForm(r: var Reader) : MalType =
  if r.tokens.len == 0:
    logger.log(lvlError, "Expect token, but got nothing")
    return MalAtom(atomType: MalNil)
  case r.peak():
    of "(": r.readList()
    of "[": r.readVector()
    of "{": r.readHashMap()
    of ")", "]", "}":
      raise newException(MalReadRBracket, r.peak())
    of "'", "`", "~", "~@", "@":
      const specialMap = {"'": "quote",
                   "`": "quasiquote",
                   "~": "unquote",
                   "~@": "splice-unquote",
                   "@": "deref"}.toTable()
      let special = r.peak()
      r.forward()
      MalList(items:
        @[MalType(MalAtom(atomType: MalSymbol, id: specialMap[special])),
          r.readForm()])
    of "^":
      r.forward()
      let
        form = r.readForm()
        meta = r.readForm()
      MalList(items:
        @[MalType(MalAtom(atomType: MalSymbol, id: "with-meta")), form, meta])
    else: r.readAtom()

proc createReader*(tokens: seq[string]) : Reader =
  Reader(tokens : tokens)

proc tokenize*(s: string) : seq[string] =
  result = @[]
  var
    p = 0
    token =
      re(r"([\s,]*" &
      r"(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*))")
    captured : array[0..1, string]
  #echo "ok"
  while p < s.len:
    let pos = find(s, token, captured, p)
    #echo fmt"{pos}, {p}, {s}, {len(s)}"
    if pos < 0 or pos >= s.len: break
    if captured[1].len == 0: break
    if captured[1][0] == ';':
      #echo "meet comment"
      p = pos + captured[0].len
      while p < s.len and s[p] != '\n':
        p += 1
      #echo "echo dumped"
    else:
      result.add(captured[1])
      p = pos + captured[0].len

proc readString*(s: string) : seq[MalType] =
  var reader: Reader = createReader(tokenize(s))
  result = @[]
  if reader.empty():
    raise newException(MalNothingToRead, "")
  while not reader.empty():
    result.add(reader.readForm())
