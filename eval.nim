import tables, logging
import definition, env
var logger = newConsoleLogger()

proc evalAST*(
  environment: var MalEnvironment,
  ast: MalType) : MalType =
  if ast of MalList:
    var l = MalList(ast)
    if l.items.len >= 1:
      l.items[0] = evalAST(environment, l.items[0])
      let head = l.items[0]
      if head of MalAtom:
        let heada = MalAtom(head)
        if heada.atomType == MalLambda:
          var isHead = true
          for i in l.items.mitems:
            if isHead:
              isHead = false
            else:
              i = evalAST(environment, i)
          return
            if l.items.len > 1:
              heada.f(l.items[1..^1])
            else:
              heada.f()
        elif heada.atomType == MalMacro:
          return
            if l.items.len > 1:
              heada.g(environment, l.items[1..^1])
            else:
              heada.g(environment)
        else:
          logger.log(lvlError, "Trying to call on a non-lambda value!")
          return MalNilAtom
    else:
      return l
  elif ast of MalVector:
    var v = MalVector(ast)
    for i in v.items.mitems:
      i = evalAST(environment, i)
    return v
  elif ast of MalHashMap:
    var m = MalHashMap(ast)
    for k, v in m.map.mpairs:
      v = evalAST(environment, v)
    return m
  elif ast of MalAtom:
    let asta = MalAtom(ast)
    if asta.atomType == MalSymbol:
      return environment.find(asta.id)
    else: return ast
