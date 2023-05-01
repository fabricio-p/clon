import colors, terminal
import ./lexer, ./ast, ./box, ./util

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
       ", col: ", $endCol,
       "}", resetStyle, ">")

const
  IndentWidth   = 2
  LabelColor    = rgb(255, 164, 0)
  IdentColor    = rgb(255, 130, 255)
  # TypeColor     = rgb(210, 0, 0)
  ExprKindColor = rgb(100, 255, 100)
  KindColor     = rgb(100, 100, 255)
  StrColor      = rgb(55, 255, 40)
  NumberColor   = rgb(255, 255, 60)
  OperatorColor = rgb(150, 170, 225)

template printIndent(f: File, indent: int): untyped =
  f.resetAttributes()
  f.setBackgroundColor(rgb(10, 10, 10))
  for i in 0..<indent:
    f.write("│")
    for j in 0..<IndentWidth:
      f.write(' ')

proc print*(f: File, fc: FcDecl|FcExpr, level: int) =
  todo("Function printing")
#[
proc print(f: File; fc: FcDecl|FcExpr; level: int) =
  f.swl("Function", resetStyle, ":")
  when fc is FcDecl:
    f.printIndent(level + 1)
    f.swl(LabelColor, "name",
          resetStyle, ": ",
          IdentColor, fc.name,
          )
  f.printIndent(level.succ)
  f.swl(LabelColor, "type", resetStyle, ":\n")
  if fc.params.len != 0:
    f.printIndent(level + 2)
    f.swl(LabelColor, "parameters", resetStyle, ":\n")
    for param in fc.params:
      f.printIndent(level + 3)
      f.sw(resetStyle, "> ",
           IdentColor, param.name,
           resetStyle, ": ",
           TypeColor)
      if param.typ.isSome():
        f.swl(param.typ.get(), )
      else:
        f.swl("<N/A>", )
  f.printIndent(level + 2)
  f.sw(LabelColor, "ret" resetStyle, ": ", TypeColor)
]#

proc print*(f: File, expr: Expr, level: int = 0) =
  f.printIndent(level)
  f.sw(fgColor)
  f.setForegroundColor(ExprKindColor)
  f.write("Expression/")
  case expr.kind
  of exprNone:
    f.swl("None")
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
    f.print(expr.fcExpr, level)
  of exprFcCall:
    f.swl("FcCall", resetStyle, ":")
    f.printIndent(level + 1)
    f.swl(LabelColor, "callee", resetStyle, ":")
    f.print(expr.fccall.callee.getRef[], level + 2)
    if expr.fccall.args.len != 0:
      f.printIndent(level + 1)
      f.swl(LabelColor, "arguments", resetStyle, ":")
      for arg in expr.fccall.args:
        f.print(arg, level + 2)
