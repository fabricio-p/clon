import sequtils, strutils
import clon/[ast, lexer, parser, printer]

when isMainModule:
  var
    line = newStringOfCap(64)
    pars: Parser
    stmt: Stmt
  while (stdout.write("clon> "); stdin.readLine(line)):
    if line.len == 0 or line.allIt(it in Whitespace): continue
    pars.lexer = initLexer(line)
    try:
      stmt = pars.parseStmt()
      stdout.print(stmt)
    except ParserError as perr:
      echo perr.msg
  echo ""
