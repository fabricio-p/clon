import clon/[ast, lexer, parser, printer]

when isMainModule:
  var
    line = newStringOfCap(64)
    pars: Parser
    stmt: Stmt
  while (stdout.write("clon> "); stdin.readLine(line)):
    pars.lexer = initLexer(line)
    stmt = pars.parseStmt()
    stdout.print(stmt)
  echo ""
