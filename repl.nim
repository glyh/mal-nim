import system, linenoise, strformat, logging, streams
import definition, reader, printer, eval, core, macros


var logger = newConsoleLogger()
var environement = defaultEnvironment

proc Read(input: string): seq[MalType] =
  result = readString(input)

proc Eval(ast: seq[MalType]): seq[MalType] =
  result = @[]
  for i in ast.items:
    result.add(eval(environement, i))

proc Print(output: seq[MalType]): string =
  result = ""
  for i in output:
    result &= $i & "\n"

proc REP(input: string): string=
  Print(Eval(Read(input)))

var s = newFileStream("./builtin.mal", fmRead)
discard Eval(Read("(list " & s.readAll() & ")"))

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
