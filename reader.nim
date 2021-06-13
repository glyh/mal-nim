import re, logging, strformat, parseutils, strutils, system
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
      result.items.add(r.readForm())
  logger.log(lvlError, "Missing right parenthesis!")

proc readAtom(r: var Reader) : MalAtom =
  let cur = r.peak()
  result =
    if match(cur, re"^[+-]?[0-9]+$"):
      var i: int64
      discard parseBiggestInt(cur, i)
      MalAtom(atomType: MalInteger, intValue: i)
    elif match(cur, re"^[+-]?\d*\.[0-9]+(e[+-]?\d+)?$"):
      var f: float64
      discard parseFloat(cur, f)
      MalAtom(atomType: MalDouble, doubleValue: f)
    elif match(cur, re"^""(\.|[^""\\])*""$"):
      MalAtom(atomType: MalString, strValue: unescape(cur))
    elif match(cur, re("^(true|false)$")):
      MalAtom(atomType: MalBool, boolValue: cur == "true")
    elif match(cur, re("^nil$")):
      MalAtom(atomType: MalNil)
    else:
      MalAtom(atomType: MalSymbol, id: cur)
  r.forward()

proc readForm(r: var Reader) : MalType =
  if r.peak()[0] == '(':
    r.readList()
  else:
    r.readAtom()

proc createReader(tokens: seq[string]) : Reader =
  Reader(tokens : tokens)

proc tokenize(s: string) : seq[string] =
  result = @[]
  var
    p = 0
    token =
      re(r"([\s,]*" &
      r"(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""?|;.*|[^\s\[\]{}('""`,;)]*))")
    captured : array[0..1, string]
  while true:
    let pos = find(s, token, captured, p)
    if pos < 0 or pos >= s.len: break
    result.add(captured[1])
    p = pos + captured[0].len

proc readString*(s: string) : MalType =
  var reader: Reader = createReader(tokenize(s))
  reader.readForm()
