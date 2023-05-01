import clon/[ast, lexer, parser, printer]

when isMainModule:
  var
    line = newStringOfCap(64)
    pars: Parser
    expr: Expr
  while (stdout.write("clon> "); stdin.readLine(line)):
    pars.lexer = initLexer(line)
    expr = pars.parseExpr()
    stdout.print(expr)
