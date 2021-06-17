import system, linenoise, strformat, logging, streams, os, strutils
import definition, reader, printer, eval, core, macros, env


var logger = newConsoleLogger()
var curEnv = defaultEnvironment
outerMost = curEnv

proc Read(input: string): seq[MalType] =
  result = readString(input)

proc Eval(ast: seq[MalType]): seq[MalType] =
  result = @[]
  for i in ast.items:
    result.add(eval(curEnv, i))

proc Print(output: seq[MalType]): string =
  result = ""
  for i in output:
    result &= $i & "\n"

proc REP(input: string): string=
  Print(Eval(Read(input)))

var s = newFileStream("./builtin.mal", fmRead)
discard Eval(Read("(list " & s.readAll() & ")"))

if os.paramCount() >= 1:
  var params = newSeq[MalType]()
  for i in 2..os.paramCount():
    params.add(MalAtom(atomType: MalString, strValue: paramStr(i)))
  curEnv.set("*ARGV*", MalList(items: params))
  stdout.write(REP(fmt"(load-file {escape(paramStr(1))})"))
else:
  while true:
    try:
      let line = readLine("user> ")
      historyAdd(line)
      stdout.write(REP($line))
    except MalNothingToRead:
      discard
    except EOFError:
      echo "Terminating"
      break
    except Exception:
      let e = getCurrentException()
      logger.log(lvlError, fmt"{e.name}: {e.msg}")
