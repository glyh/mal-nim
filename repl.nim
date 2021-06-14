import system, linenoise
import definition, reader, printer
proc Read(input: string): MalType =
  readString(input)

proc Eval(ast: MalType): MalType =
  ast

proc Print(output: MalType): string =
  $output

proc REP(input: string): string=
  Print (Eval (Read (input)))

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
