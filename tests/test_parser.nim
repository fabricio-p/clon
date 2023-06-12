import unittest
include clon/[parser, printer]

suite "clon/parser":
  suite "expressions":
    suite "expressions":
      var
        parser: Parser
        expr: Expr
      test "integer":
        parser.lexer = initLexer("12")
        expr = parser.parseExpr()
        check expr.kind == exprLit
        check expr.lit.kind == litInt
        check expr.lit.i == 12
      test "character":
        parser.lexer = initLexer("'f'")
        expr = parser.parseExpr()
        check expr.kind == exprLit
        check expr.lit.kind == litChr
        check expr.lit.c == 'f'
      test "float":
        parser.lexer = initLexer("43.7658")
        expr = parser.parseExpr()
        check expr.kind == exprLit
        check expr.lit.kind == litFloat
        check expr.lit.f == 43.7658
      test "string":
        parser.lexer = initLexer""""foo bar \n \r""""
        expr = parser.parseExpr()
        check expr.kind == exprLit
        check expr.lit.kind == litStr
        check expr.lit.s == "foo bar \n \r"
      test "identifiers":
        parser.lexer = initLexer("abcDef_GDU")
        expr = parser.parseExpr()
        check expr.kind == exprIdent
        check expr.ident == "abcDef_GDU"
    suite "prefix operators":
      var
        parser: Parser
        expr: Expr
      test "minus":
        parser.lexer = initLexer("- 1223")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opMinus
        check expr.op.operands[0].kind == exprLit
        check expr.op.operands[0].lit.kind == litInt
        check expr.op.operands[0].lit.i == 1223
      test "not":
        parser.lexer = initLexer("not 1")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opNot
        check expr.op.operands[0].kind == exprLit
        check expr.op.operands[0].lit.kind == litInt
        check expr.op.operands[0].lit.i == 1
    suite "infix operators":
      var
        parser: Parser
        expr: Expr
      test "terms":
        parser.lexer = initLexer("a + b")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opPlus
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
        #
        parser.lexer = initLexer("1 + 2 + 3 + 4 + 5 + 6 + 7")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opPlus
        check expr.op.operands.len == 7
        for (i, operand) in expr.op.operands.pairs:
          check operand.kind == exprLit
          check operand.lit.kind == litInt
          check operand.lit.i == i + 1
        #
        parser.lexer = initLexer("a - b")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opMinus
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
      test "factor":
        parser.lexer = initLexer("a - b * 4")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opMinus
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprOp
        check expr.op.operands[1].op.kind == opAsterisk
        check expr.op.operands[1].op.operands.len == 2
        check expr.op.operands[1].op.operands[0].kind == exprIdent
        check expr.op.operands[1].op.operands[0].ident == "b"
        check expr.op.operands[1].op.operands[1].kind == exprLit
        check expr.op.operands[1].op.operands[1].lit.kind == litInt
        check expr.op.operands[1].op.operands[1].lit.i == 4
        parser.lexer = initLexer("(1 + 2) / (3 - fooBar)")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opSlash
        check expr.op.operands[0].kind == exprOp
        check expr.op.operands[0].op.kind == opPlus
        check expr.op.operands[0].op.operands[0].kind == exprLit
        check expr.op.operands[0].op.operands[0].lit.kind == litInt
        check expr.op.operands[0].op.operands[0].lit.i == 1
        check expr.op.operands[0].op.operands[1].kind == exprLit
        check expr.op.operands[0].op.operands[1].lit.kind == litInt
        check expr.op.operands[0].op.operands[1].lit.i == 2
        check expr.op.operands[1].kind == exprOp
        check expr.op.operands[1].op.kind == opMinus
        check expr.op.operands[1].op.operands[0].kind == exprLit
        check expr.op.operands[1].op.operands[0].lit.kind == litInt
        check expr.op.operands[1].op.operands[0].lit.i == 3
        check expr.op.operands[1].op.operands[1].kind == exprIdent
        check expr.op.operands[1].op.operands[1].ident == "fooBar"
      test "assign":
        parser.lexer = initLexer("a = b")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opAssign
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
        #
        parser.lexer = initLexer("a = b + c = d")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opPlus
        check expr.op.operands[0].kind == exprOp
        check expr.op.operands[0].op.kind == opAssign
        check expr.op.operands[0].op.operands[0].kind == exprIdent
        check expr.op.operands[0].op.operands[0].ident == "a"
        check expr.op.operands[0].op.operands[1].kind == exprIdent
        check expr.op.operands[0].op.operands[1].ident == "b"
        check expr.op.operands[1].kind == exprOp
        check expr.op.operands[1].op.kind == opAssign
        check expr.op.operands[1].op.operands[0].kind == exprIdent
        check expr.op.operands[1].op.operands[0].ident == "c"
        check expr.op.operands[1].op.operands[1].kind == exprIdent
        check expr.op.operands[1].op.operands[1].ident == "d"
      test "index":
        parser.lexer = initLexer("a[b]")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opIndex
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
      test "dot":
        parser.lexer = initLexer("a.b")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opDot
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
        #
        parser.lexer = initLexer("a.b.def")
        expr = parser.parseExpr()
        check expr.kind == exprOp
        check expr.op.kind == opDot
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprIdent
        check expr.op.operands[1].ident == "b"
        check expr.op.operands[2].kind == exprIdent
        check expr.op.operands[2].ident == "def"
    suite "function call":
      var
        parser: Parser
        expr: Expr
      test "without arguments":
        parser.lexer = initLexer("a()")
        expr = parser.parseExpr()
        check expr.kind == exprFcCall
        check expr.fcCall.callee.kind == exprIdent
        check expr.fcCall.callee.ident == "a"
        check expr.fcCall.args.len == 0
      test "with arguments":
        parser.lexer = initLexer("a(bc, 1234)")
        expr = parser.parseExpr()
        check expr.kind == exprFcCall
        check expr.fcCall.callee.kind == exprIdent
        check expr.fcCall.callee.ident == "a"
        check expr.fcCall.args.len == 2
        check expr.fcCall.args[0].kind == exprIdent
        check expr.fcCall.args[0].ident == "bc"
        check expr.fcCall.args[1].kind == exprLit
        check expr.fcCall.args[1].lit.kind == litInt
        check expr.fcCall.args[1].lit.i == 1234
      test "call as argument":
        parser.lexer = initLexer("a(b(c), d(e, f))")
        expr = parser.parseExpr()
        check expr.kind == exprFcCall
        check expr.fcCall.callee.kind == exprIdent
        check expr.fcCall.callee.ident == "a"
        check expr.fcCall.args[0].kind == exprFcCall
        check expr.fcCall.args[0].fcCall.callee.kind == exprIdent
        check expr.fcCall.args[0].fcCall.callee.ident == "b"
        check expr.fcCall.args[0].fcCall.args.len == 1
        check expr.fcCall.args[0].fcCall.args[0].kind == exprIdent
        check expr.fcCall.args[0].fcCall.args[0].ident == "c"
        check expr.fcCall.args[1].kind == exprFcCall
        check expr.fcCall.args[1].fcCall.callee.kind == exprIdent
        check expr.fcCall.args[1].fcCall.callee.ident == "d"
        check expr.fcCall.args[1].fcCall.args.len == 2
        check expr.fcCall.args[1].fcCall.args[0].kind == exprIdent
        check expr.fcCall.args[1].fcCall.args[0].ident == "e"
        check expr.fcCall.args[1].fcCall.args[1].kind == exprIdent
        check expr.fcCall.args[1].fcCall.args[1].ident == "f"
