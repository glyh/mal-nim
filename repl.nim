import system, linenoise, tables
import definition, reader, printer, eval, env, macros

var environement = defaultEnvironment
proc Read(input: string): MalType =
  readString(input)

proc Eval(ast: MalType): MalType =
  evalAST(environement, ast)

proc Print(output: MalType): string =
  $output

proc REP(input: string): string=
  Print(Eval(Read(input)))

initMacros(environement)
while true:
  try:
    let line = readLine("user> ")
    historyAdd(line)
    echo REP($line)
  except MalNothingToRead:
    discard
  except EOFError:
    echo "Terminating"
    break
