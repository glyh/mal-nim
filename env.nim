import tables, strformat
import definition

proc set*(env: var MalEnvironment, symbol: string, value: MalType) =
  env.symbols[symbol] = value

proc get*(env: MalEnvironment, symbol: string) : MalEnvironment =
  var e = env
  while e.outer != nil and not e.symbols.hasKey(symbol): e = e.outer
  return e

proc find*(env: MalEnvironment, symbol: string) : MalType =
  var e = get(env, symbol)
  if e.symbols.hasKey(symbol): e.symbols[symbol]
  else:
    raise newException(
      IndexDefect,
      fmt"The symbol ""{symbol}"" doesn't exist in this context")

proc doBind*(env: var MalEnvironment, binds: seq[string], exprs: seq[MalType], Varargs: bool) =
  if binds.len == exprs.len or (Varargs and binds.len <= exprs.len + 1):
    for i in 0..binds.high:
      if i == binds.high and Varargs:
        env.set(binds[i], MalList(items: exprs[i+1..^1]))
      else:
        env.set(binds[i], exprs[i])
  else:
    raise newException(MalSyntaxError, "Syntax error while parsing `fn*`")
