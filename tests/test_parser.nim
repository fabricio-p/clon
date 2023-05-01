import unittest
import clon/[ast, lexer, parser]

test "simple expressions":
  var
    parser: Parser
    expr: Expr
  #
  parser.lexer = initLexer("12")
  expr = parser.parseExpr()
  assert expr.kind == exprLit
  assert expr.lit.kind == litInt
  assert expr.lit.i == 12
  #
  parser.lexer = initLexer("'f'")
  expr = parser.parseExpr()
  assert expr.kind == exprLit
  assert expr.lit.kind == litChr
  assert expr.lit.c == 'f'
  #
  parser.lexer = initLexer("43.7658")
  expr = parser.parseExpr()
  assert expr.kind == exprLit
  assert expr.lit.kind == litFloat
  assert expr.lit.f == 43.7658
  #
  parser.lexer = initLexer""""foo bar \n \r""""
  expr = parser.parseExpr()
  assert expr.kind == exprLit
  assert expr.lit.kind == litStr
  assert expr.lit.s == "foo bar \n \r"
  #
  parser.lexer = initLexer("abcDef_GDU")
  expr = parser.parseExpr()
  assert expr.kind == exprIdent
  assert expr.ident == "abcDef_GDU"

test "prefix operators":
  var
    parser: Parser
    expr: Expr
  #
  parser.lexer = initLexer("- 1223")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opMinus
  assert expr.op.operands[0].kind == exprLit
  assert expr.op.operands[0].lit.kind == litInt
  assert expr.op.operands[0].lit.i == 1223

test "infix operators":
  var
    parser: Parser
    expr: Expr
  #
  parser.lexer = initLexer("a + b")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opPlus
  assert expr.op.operands[0].kind == exprIdent
  assert expr.op.operands[0].ident == "a"
  assert expr.op.operands[1].kind == exprIdent
  assert expr.op.operands[1].ident == "b"
  #
  parser.lexer = initLexer("a - b * 4")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opMinus
  assert expr.op.operands[0].kind == exprIdent
  assert expr.op.operands[0].ident == "a"
  assert expr.op.operands[1].kind == exprOp
  assert expr.op.operands[1].op.kind == opAsterisk
  assert expr.op.operands[1].op.operands[0].kind == exprIdent
  assert expr.op.operands[1].op.operands[0].ident == "b"
  assert expr.op.operands[1].op.operands[1].kind == exprLit
  assert expr.op.operands[1].op.operands[1].lit.kind == litInt
  assert expr.op.operands[1].op.operands[1].lit.i == 4
  #
  parser.lexer = initLexer("(1 + 2) / (3 - fooBar)")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opSlash
  assert expr.op.operands[0].kind == exprOp
  assert expr.op.operands[0].op.kind == opPlus
  assert expr.op.operands[0].op.operands[0].kind == exprLit
  assert expr.op.operands[0].op.operands[0].lit.kind == litInt
  assert expr.op.operands[0].op.operands[0].lit.i == 1
  assert expr.op.operands[0].op.operands[1].kind == exprLit
  assert expr.op.operands[0].op.operands[1].lit.kind == litInt
  assert expr.op.operands[0].op.operands[1].lit.i == 2
  assert expr.op.operands[1].kind == exprOp
  assert expr.op.operands[1].op.kind == opMinus
  assert expr.op.operands[1].op.operands[0].kind == exprLit
  assert expr.op.operands[1].op.operands[0].lit.kind == litInt
  assert expr.op.operands[1].op.operands[0].lit.i == 3
  assert expr.op.operands[1].op.operands[1].kind == exprIdent
  assert expr.op.operands[1].op.operands[1].ident == "fooBar"
  #
  parser.lexer = initLexer("1 + 2 + 3 + 4 + 5 + 6 + 7")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opPlus
  for (i, operand) in expr.op.operands.pairs:
    assert operand.kind == exprLit
    assert operand.lit.kind == litInt
    assert operand.lit.i == i + 1
  #
  parser.lexer = initLexer("a = b")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opAssign
  assert expr.op.operands[0].kind == exprIdent
  assert expr.op.operands[0].ident == "a"
  assert expr.op.operands[1].kind == exprIdent
  assert expr.op.operands[1].ident == "b"
  #
  parser.lexer = initLexer("a = b + c = d")
  expr = parser.parseExpr()
  assert expr.kind == exprOp
  assert expr.op.kind == opPlus
  assert expr.op.operands[0].kind == exprOp
  assert expr.op.operands[0].op.kind == opAssign
  assert expr.op.operands[0].op.operands[0].kind == exprIdent
  assert expr.op.operands[0].op.operands[0].ident == "a"
  assert expr.op.operands[0].op.operands[1].kind == exprIdent
  assert expr.op.operands[0].op.operands[1].ident == "b"
  assert expr.op.operands[1].kind == exprOp
  assert expr.op.operands[1].op.kind == opAssign
  assert expr.op.operands[1].op.operands[0].kind == exprIdent
  assert expr.op.operands[1].op.operands[0].ident == "c"
  assert expr.op.operands[1].op.operands[1].kind == exprIdent
  assert expr.op.operands[1].op.operands[1].ident == "d"

test "function call":
  discard
