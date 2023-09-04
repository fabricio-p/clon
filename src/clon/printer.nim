import colors, terminal
import questionable/options
import ./lexer, ./ast, ./box

enableTrueColors()

template sw(f: File, argv: varargs[untyped]): untyped =
  f.styledWrite(argv)

template swl(f: File, argv: varargs[untyped]): untyped =
  f.styledWriteLine(argv)

proc print*(f: File, token: Token, src: string) =
  f.sw("<", styleBright, fgRed, $token.kind, " ", )
  if token.kind in {tokIdent, tokStrLit, tokIntLit, tokFloatLit, tokChrLit}:
    f.sw(fgCyan, src[token.span.s.offset..<token.span.e.offset],
         resetStyle, " ")
  var
    startCol = 1
    endCol = 1
  for i in countdown(token.span.s.offset, 1):
    if src[i] == '\n': break
    inc startCol
  if token.span.s.line == token.span.e.line:
    endCol = token.span.e.offset - token.span.s.offset + startCol
  else:
    for i in countdown(token.span.e.offset, 1):
      if src[i] == '\n': break
      inc endCol
  f.sw(rgb(55, 20, 60),
       "{offset: ", $token.span.s.offset,
       ", line: ", $token.span.s.line,
       ", col: ", $startCol,
       "}...{offset: ", $token.span.e.offset,
       ", line: ", $token.span.e.line,
       ", col: ", $endCol, "}",
       resetStyle, ">")

const
  IndentWidth   = 2
  # general
  IndentBgColor = rgb(10, 10, 10)
  LabelColor    = rgb(255, 164,   0)
  IdentColor    = rgb(255, 130, 255)
  # expressions
  ExprKindColor = rgb(110, 255, 190)
  KindColor     = rgb(100, 100, 255)
  StrColor      = rgb( 30, 200,  20)
  NumberColor   = rgb(255, 255,  60)
  OperatorColor = rgb(150, 170, 225)
  # statements
  StmtKindColor = rgb(255, 100, 100)

template printIndent(f: File, indent: int): untyped =
  f.resetAttributes()
  f.setBackgroundColor(IndentBgColor)
  for i in 0..<indent:
    f.write("┃")
    for j in 0 ..< IndentWidth - 1:
      f.write(' ')
  f.resetAttributes()

proc print*(f: File, expr: Expr, level: int = 0)
proc print*(f: File, stmt: Stmt, level: int = 0)

proc print(f: File; fc: FcDecl|FcExpr; level: int = 0) =
  f.swl("Function", resetStyle, ":")
  when fc is FcDecl:
    f.printIndent(level + 1)
    f.swl(LabelColor, "name",
          resetStyle, ": ",
          IdentColor, fc.name)
  f.printIndent(level.succ)
  f.swl(LabelColor, "type", resetStyle, ":")
  f.printIndent(level + 2)
  f.sw(LabelColor, "parameters", resetStyle, ":")
  if fc.params.len != 0:
    f.write('\n')
    for param in fc.params:
      f.printIndent(level + 2)
      f.sw(bgWhite, " ", resetStyle,
           IdentColor, param.name,
           resetStyle, ":")
      if param.typ.kind != exprNone:
        f.write('\n')
        f.print(param.typ, level + 3)
      else:
        f.swl(" ", KindColor, "<N/A>")
  else:
    f.swl(" ", KindColor, "-")
  f.printIndent(level + 2)
  f.sw(LabelColor, "ret", resetStyle, ":")
  if fc.ret.isEmpty():
    f.swl(" ", KindColor, "-")
  else:
    f.write('\n')
    f.print(fc.ret.getRefUnsafe()[], level + 3)
  f.printIndent(level + 1)
  f.swl(LabelColor, "body", resetStyle, ":")
  if fc.body.code.len == 0:
    f.print(Stmt(kind: stmtNone), level + 2)
  else:
    for bodyStmt in fc.body.code:
      f.print(bodyStmt, level + 2)

proc print*(f: File, expr: Expr, level: int = 0) =
  f.printIndent(level)
  f.sw(fgColor)
  f.setForegroundColor(ExprKindColor)
  f.write("Expression/")
  case expr.kind
  of exprNone:
    f.swl("None", resetStyle)
  of exprLit:
    f.swl("Literal", resetStyle, ": ",
          KindColor, $expr.lit.kind,
          resetStyle, "(",
          if expr.lit.kind == litStr: StrColor else: NumberColor, $expr.lit,
          resetStyle, ")")
  of exprIdent:
    f.swl("Identifier", resetStyle, ": ", IdentColor, expr.ident)
  of exprOp:
    f.swl("Operation", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "operator",
          resetStyle, ": ",
          OperatorColor, $expr.op.kind)
    f.printIndent(level + 1)
    f.swl(LabelColor, "operands", resetStyle, ":")
    for operand in expr.op.operands:
      f.print(operand, level + 2)
  of exprFc:
    f.print(expr.fc, level)
  of exprFcCall:
    f.swl("FcCall", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "callee", resetStyle, ":")
    f.print(expr.fccall.callee.getRefUnsafe[], level + 2)
    if expr.fccall.args.len != 0:
      f.printIndent(level + 1)
      f.swl(LabelColor, "arguments", resetStyle, ":")
      for arg in expr.fccall.args:
        f.print(arg, level + 2)
  of exprFcType:
    f.swl("FcType", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "parameters", resetStyle, ":")
    if expr.fcType.params.len == 0:
      f.printIndent(level + 2)
      f.swl(": -")
    else:
      for param in expr.fcType.params:
        f.print(param, level + 2)
    f.swl(LabelColor, "return", resetStyle, ":")
    if expr.fcType.ret.isEmpty():
      f.printIndent(level + 2)
      f.swl(": -")
    else:
        f.print(expr.fcType.ret.getRefUnsafe[], level + 2)
  of exprBracket:
    f.swl("Bracket", resetStyle, ":")
    if expr.bracket.len == 0:
      f.print(Expr(kind: exprNone), level + 1)
    else:
      for bracketItem in expr.bracket:
        f.print(bracketItem, level + 1)
  of exprBoxVal:
    f.swl("BoxVal", resetStyle, ":")
    if expr.boxVal.len == 0:
      f.print(Expr(kind: exprNone), level + 1)
    else:
      for (key, value) in expr.boxVal.items:
        let valueLevel =
          if keyVal =? key:
            f.printIndent(level + 1)
            f.swl(StrColor, keyVal, resetStyle, ": ")
            level + 2
          else:
            level + 1
        f.print(value, valueLevel)

proc print*(f: File, stmt: Stmt, level: int = 0) =
  f.printIndent(level)
  f.sw(fgColor)
  f.setForegroundColor(StmtKindColor)
  f.write("Statement/")
  case stmt.kind
  of stmtNone:
    f.swl("None", resetStyle)
  of stmtExpr:
    f.swl("Expression", resetStyle, ":")
    f.print(stmt.expr, level + 1)
  of stmtVarDecl:
    f.swl("Variable", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "name", resetStyle, ": ", IdentColor, stmt.varDecl.name)
    f.printIndent(level + 1)
    f.sw(LabelColor, "type", resetStyle, ":")
    if stmt.varDecl.typ.kind == exprNone:
      f.swl(" <N/A>")
    else:
      f.write('\n')
      f.print(stmt.varDecl.typ, level + 2)
    f.printIndent(level + 1)
    f.sw(LabelColor, "value", resetStyle, ":")
    if stmt.varDecl.value.kind == exprNone:
      f.swl(" <N/A>")
    else:
      f.write('\n')
      f.print(stmt.varDecl.value, level + 2)
  of stmtIf:
    f.sw("If", resetStyle, ":")
    if stmt.ifs.len == 0:
      f.swl(" ", LabelColor, "Empty")
    else:
      f.write('\n')
      for (i, ifClause) in stmt.ifs.pairs:
        f.printIndent(level + 1)
        f.swl(bgWhite, " ", resetStyle,
              LabelColor, "clause",
              resetStyle, " [", $i, "]:")
        f.printIndent(level + 2)
        f.swl(LabelColor, "condition", resetStyle, ":")
        f.print(ifClause.cond, level + 3)
        f.printIndent(level + 2)
        f.swl(LabelColor, "body", resetStyle, ":")
        if ifClause.body.code.len == 0:
          f.print(Stmt(kind: stmtNone), level + 3)
        else:
          for bodyItem in ifClause.body.code:
            f.print(bodyItem, level + 3)
  of stmtForInLoop:
    f.swl("ForIn", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "capture",
          resetStyle, ": ",
          IdentColor, stmt.forinl.capture)
    f.printIndent(level + 1)
    f.swl(LabelColor, "iterator", resetStyle, ":")
    f.print(stmt.forinl.iter, level + 2)
    f.printIndent(level + 1)
    f.swl(LabelColor, "body", resetStyle, ":")
    if stmt.forinl.body.code.len == 0:
      f.print(Stmt(kind: stmtNone), level + 2)
    else:
      for bodyItem in stmt.forinl.body.code:
        f.print(bodyItem, level + 2)
  of stmtWhlLoop:
    f.swl("While", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "condition", resetStyle, ":")
    f.print(stmt.whll.cond, level + 2)
    f.printIndent(level + 1)
    f.swl(LabelColor, "body", resetStyle, ":")
    if stmt.whll.body.code.len == 0:
      f.print(Stmt(kind: stmtNone), level + 2)
    else:
      for bodyItem in stmt.whll.body.code:
        f.print(bodyItem, level + 2)
  of stmtForLoop:
    f.swl("For", resetStyle, ":")
    f.printIndent(level + 1)
    f.sw(LabelColor, "init", resetStyle, ":")
    if stmt.forl.init.isEmpty():
      f.print(Stmt(kind: stmtNone), level + 2)
    else:
      f.print(stmt.forl.init.getRefUnsafe()[])
    f.printIndent(level + 1)
    f.sw(LabelColor, "condition", resetStyle, ":")
    f.print(stmt.forl.cond, level + 2)
    f.printIndent(level + 1)
    f.swl(LabelColor, "body", ":")
    if stmt.forl.body.code.len == 0:
      f.print(Stmt(kind: stmtNone), level + 1)
    else:
      for bodyItem in stmt.forl.body.code:
        f.print(bodyItem, level + 2)
  of stmtFcDecl:
    f.print(stmt.fcDecl, level)
  of stmtRet:
    f.swl("Return", resetStyle, ":")
    f.print(stmt.ret, level + 1)
