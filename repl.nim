import system, linenoise
import definition, reader, printer
proc Read(input: string): MalType =
  readString(input)

proc Eval(input: MalType): MalType =
  input

proc Print(input: MalType): string =
  $input

proc REP(input: string): string=
  Print (Eval (Read (input)))

while true:
  try:
    let line = readLine("user> ")
    historyAdd(line)
    echo REP($line)
  except EOFError:
    echo "Terminating"
    break
