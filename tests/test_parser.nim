import unittest
include clon/[parser, printer]

suite "clon/parser":
  suite "expressions":
    suite "primitives":
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
        check expr.op.kind == opAssign
        check expr.op.operands[0].kind == exprIdent
        check expr.op.operands[0].ident == "a"
        check expr.op.operands[1].kind == exprOp
        check expr.op.operands[1].op.kind == opPlus
        check expr.op.operands[1].op.operands[0].kind == exprIdent
        check expr.op.operands[1].op.operands[0].ident == "b"
        check expr.op.operands[1].op.operands[1].kind == exprOp
        check expr.op.operands[1].op.operands[1].op.kind == opAssign
        check expr
                .op
                .operands[1]
                .op
                .operands[1]
                .op
                .operands[0]
                .kind == exprIdent
        check expr.op.operands[1].op.operands[1].op.operands[0].ident == "c"
        check expr
                .op
                .operands[1]
                .op
                .operands[1]
                .op
                .operands[1]
                .kind == exprIdent
        check expr.op.operands[1].op.operands[1].op.operands[1].ident == "d"
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
    suite "function expression":
      var
        parser: Parser
        expr: Expr
      test "no parameters, no return":
        parser.lexer = initLexer("fc() end")
        expr = parser.parseExpr()
        check expr.kind == exprFc
        check expr.fc.params.len == 0
        check expr.fc.ret.isEmpty()
        check expr.fc.body.code.len == 0
      test "no parameters, with return":
        parser.lexer = initLexer("fc(): void end")
        expr = parser.parseExpr()
        check expr.kind == exprFc
        check expr.fc.params.len == 0
        check expr.fc.ret.kind == exprIdent
        check expr.fc.ret.ident == "void"
        check expr.fc.body.code.len == 0
      test "with parameters, no return":
        parser.lexer = initLexer("fc(a:int, b:int) end")
        expr = parser.parseExpr()
        check expr.kind == exprFc
        check expr.fc.params.len == 2
        check expr.fc.params[0].name == "a"
        check expr.fc.params[0].typ.kind == exprIdent
        check expr.fc.params[0].typ.ident == "int"
        check expr.fc.params[1].name == "b"
        check expr.fc.params[1].typ.kind == exprIdent
        check expr.fc.params[1].typ.ident == "int"
        check expr.fc.ret.isEmpty()
        check expr.fc.body.code.len == 0
      test "with parameters, with return":
        parser.lexer = initLexer("fc(foo: string): LinkedList[char] end")
        expr = parser.parseExpr()
        check expr.kind == exprFc
        check expr.fc.params.len == 1
        check expr.fc.params[0].name == "foo"
        check expr.fc.params[0].typ.kind == exprIdent
        check expr.fc.params[0].typ.ident == "string"
        check expr.fc.ret.kind == exprOp
        check expr.fc.ret.op.kind == opIndex
        check expr.fc.ret.op.operands.len == 2
        check expr.fc.ret.op.operands[0].kind == exprIdent
        check expr.fc.ret.op.operands[0].ident == "LinkedList"
        check expr.fc.ret.op.operands[1].kind == exprIdent
        check expr.fc.ret.op.operands[1].ident == "char"
        check expr.fc.body.code.len == 0
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
      test "call function expression":
        parser.lexer = initLexer("(fc() end)()")
        expr = parser.parseExpr()
        check expr.kind == exprFcCall
        check expr.fcCall.callee.kind == exprFc
        check expr.fcCall.callee.fc.params.len == 0
        check expr.fcCall.callee.fc.ret.isEmpty()
        check expr.fcCall.callee.fc.body.code.len == 0
        check expr.fcCall.args.len == 0
  suite "statements":
    suite "loc":
      var
        parser: Parser
        stmt: Stmt
      test "uninitialized":
        parser.lexer = initLexer("loc a: b;")
        stmt = parser.parseStmt()
        check stmt.kind == stmtVarDecl
        check stmt.varDecl.name == "a"
        check stmt.varDecl.typ.kind == exprIdent
        check stmt.varDecl.typ.ident == "b"
        check stmt.varDecl.value.kind == exprNone
      test "initialized":
        parser.lexer = initLexer("loc i: int = 0;")
        stmt = parser.parseStmt()
        check stmt.kind == stmtVarDecl
        check stmt.varDecl.name == "i"
        check stmt.varDecl.typ.kind == exprIdent
        check stmt.varDecl.typ.ident == "int"
        check stmt.varDecl.value.kind == exprLit
        check stmt.varDecl.value.lit.kind == litInt
        check stmt.varDecl.value.lit.i == 0
    suite "if":
      var
        parser: Parser
        stmt: Stmt
      test "single clause":
        parser.lexer = initLexer("if ?(cond) loc k: d = dhp; end")
        stmt = parser.parseStmt()
        check stmt.kind == stmtIf
        check stmt.ifs.len == 1
        check stmt.ifs[0].cond.kind == exprIdent
        check stmt.ifs[0].cond.ident == "cond"
        check stmt.ifs[0].body.code.len == 1
        check stmt.ifs[0].body.code[0].kind == stmtVarDecl
        check stmt.ifs[0].body.code[0].varDecl.name == "k"
        check stmt.ifs[0].body.code[0].varDecl.typ.kind == exprIdent
        check stmt.ifs[0].body.code[0].varDecl.typ.ident == "d"
        check stmt.ifs[0].body.code[0].varDecl.value.kind == exprIdent
        check stmt.ifs[0].body.code[0].varDecl.value.ident == "dhp"
      test "multiple clauses":
        parser.lexer = initLexer("""
        if
        ?(cond1)
           foo = 1;
        ?((cond2))
           foo = 30;
        end
        """)
        stmt = parser.parseStmt()
        check stmt.kind == stmtIf
        check stmt.ifs.len == 2
        check stmt.ifs[0].cond.kind == exprIdent
        check stmt.ifs[0].cond.ident == "cond1"
        check stmt.ifs[0].body.code.len == 1
        check stmt.ifs[0].body.code[0].kind == stmtExpr
        check stmt.ifs[0].body.code[0].expr.kind == exprOp
        check stmt.ifs[0].body.code[0].expr.op.kind == opAssign
        check stmt.ifs[0].body.code[0].expr.op.operands[0].kind == exprIdent
        check stmt.ifs[0].body.code[0].expr.op.operands[0].ident == "foo"
        check stmt.ifs[0].body.code[0].expr.op.operands[1].kind == exprLit
        check stmt.ifs[0].body.code[0].expr.op.operands[1].lit.kind == litInt
        check stmt.ifs[0].body.code[0].expr.op.operands[1].lit.i == 1
        check stmt.ifs[1].cond.kind == exprIdent
        check stmt.ifs[1].cond.ident == "cond2"
        check stmt.ifs[1].body.code.len == 1
        check stmt.ifs[1].body.code[0].kind == stmtExpr
        check stmt.ifs[1].body.code[0].expr.kind == exprOp
        check stmt.ifs[1].body.code[0].expr.op.kind == opAssign
        check stmt.ifs[1].body.code[0].expr.op.operands[0].kind == exprIdent
        check stmt.ifs[1].body.code[0].expr.op.operands[0].ident == "foo"
        check stmt.ifs[1].body.code[0].expr.op.operands[1].kind == exprLit
        check stmt.ifs[1].body.code[0].expr.op.operands[1].lit.kind == litInt
        check stmt.ifs[1].body.code[0].expr.op.operands[1].lit.i == 30
      test "with else clause":
        parser.lexer = initLexer("""
        if
        ?(cond)
           body;
        ?()
           elsebody;
        end
        """)
        stmt = parser.parseStmt()
        check stmt.kind == stmtIf
        check stmt.ifs.len == 2
        check stmt.ifs[0].cond.kind == exprIdent
        check stmt.ifs[0].cond.ident == "cond"
        check stmt.ifs[0].body.code.len == 1
        check stmt.ifs[0].body.code[0].kind == stmtExpr
        check stmt.ifs[0].body.code[0].expr.kind == exprIdent
        check stmt.ifs[0].body.code[0].expr.ident == "body"
        check stmt.ifs[1].cond.kind == exprNone
        check stmt.ifs[1].body.code.len == 1
        check stmt.ifs[1].body.code[0].kind == stmtExpr
        check stmt.ifs[1].body.code[0].expr.kind == exprIdent
        check stmt.ifs[1].body.code[0].expr.ident == "elsebody"
    suite "for":
      var
        parser: Parser
        stmt: Stmt
      test "classic for":
        parser.lexer = initLexer("""
        for (loc i:int = 0; i < n; i = i + 1)
           foo;
        end
        """)
        stmt = parser.parseStmt()
        check stmt.kind == stmtForLoop
        # .init
        #   .name
        check stmt.forl.init.kind == stmtVarDecl
        check stmt.forl.init.varDecl.name == "i"
        #   .typ
        check stmt.forl.init.varDecl.typ.kind == exprIdent
        check stmt.forl.init.varDecl.typ.ident == "int"
        #   .value
        check stmt.forl.init.varDecl.value.kind == exprLit
        check stmt.forl.init.varDecl.value.lit.kind == litInt
        check stmt.forl.init.varDecl.value.lit.i == 0
        # .cond
        check stmt.forl.cond.kind == exprOp
        check stmt.forl.cond.op.kind == opLt
        check stmt.forl.cond.op.operands.len == 2
        check stmt.forl.cond.op.operands[0].kind == exprIdent
        check stmt.forl.cond.op.operands[0].ident == "i"
        check stmt.forl.cond.op.operands[1].kind == exprIdent
        check stmt.forl.cond.op.operands[1].ident == "n"
        # .step
        check stmt.forl.step.kind == exprOp
        check stmt.forl.step.op.kind == opAssign
        check stmt.forl.step.op.operands.len == 2
        check stmt.forl.step.op.operands[0].kind == exprIdent
        check stmt.forl.step.op.operands[0].ident == "i"
        check stmt.forl.step.op.operands[1].kind == exprOp
        check stmt.forl.step.op.operands[1].op.kind == opPlus
        check stmt.forl.step.op.operands[1].op.operands.len == 2
        check stmt.forl.step.op.operands[1].op.operands[0].kind == exprIdent
        check stmt.forl.step.op.operands[1].op.operands[0].ident == "i"
        check stmt.forl.step.op.operands[1].op.operands[1].kind == exprLit
        check stmt.forl.step.op.operands[1].op.operands[1].lit.kind == litInt
        check stmt.forl.step.op.operands[1].op.operands[1].lit.i == 1
        check stmt.forl.body.code.len == 1
        check stmt.forl.body.code[0].kind == stmtExpr
        check stmt.forl.body.code[0].expr.kind == exprIdent
        check stmt.forl.body.code[0].expr.ident == "foo"
      test "for..in":
        parser.lexer = initLexer("""
        for (item in array)
           print(item);
        end
        """)
        stmt = parser.parseStmt()
        check stmt.kind == stmtForInLoop
        check stmt.forinl.capture == "item"
        check stmt.forinl.iter.kind == exprIdent
        check stmt.forinl.iter.ident == "array"
        check stmt.forinl.body.code.len == 1
        check stmt.forinl.body.code[0].kind == stmtExpr
        check stmt.forinl.body.code[0].expr.kind == exprFcCall
        check stmt.forinl.body.code[0].expr.fcCall.callee.kind == exprIdent
        check stmt.forinl.body.code[0].expr.fcCall.callee.ident == "print"
        check stmt.forinl.body.code[0].expr.fcCall.args[0].kind == exprIdent
        check stmt.forinl.body.code[0].expr.fcCall.args[0].ident == "item"
    suite "return":
      var
        parser: Parser
        stmt: Stmt
      test "with value":
        parser.lexer = initLexer("ret foo;")
        stmt = parser.parseStmt()
        check stmt.kind == stmtRet
        check stmt.ret.kind == exprIdent
        check stmt.ret.ident == "foo"
      test "without value":
        parser.lexer = initLexer("ret;")
        stmt = parser.parseStmt()
        check stmt.kind == stmtRet
        check stmt.ret.kind == exprNone
    suite "fc":
      var
        parser: Parser
        stmt: Stmt
      test "no parameters, no return":
        parser.lexer = initLexer("fc foo() end")
        stmt = parser.parseStmt()
        check stmt.kind == stmtFcDecl
        check stmt.fcDecl.name == "foo"
        check stmt.fcDecl.params.len == 0
        check stmt.fcDecl.ret.isEmpty()
        check stmt.fcDecl.body.code.len == 0
      test "no parameters, with return":
        parser.lexer = initLexer("fc bar(): void end")
        stmt = parser.parseStmt()
        check stmt.kind == stmtFcDecl
        check stmt.fcDecl.name == "bar"
        check stmt.fcDecl.params.len == 0
        check stmt.fcDecl.ret.kind == exprIdent
        check stmt.fcDecl.ret.ident == "void"
        check stmt.fcDecl.body.code.len == 0
      test "with parameters, no return":
        parser.lexer = initLexer("fc baz(a:int, b:int) end")
        stmt = parser.parseStmt()
        check stmt.kind == stmtFcDecl
        check stmt.fcDecl.name == "baz"
        check stmt.fcDecl.params.len == 2
        check stmt.fcDecl.params[0].name == "a"
        check stmt.fcDecl.params[0].typ.kind == exprIdent
        check stmt.fcDecl.params[0].typ.ident == "int"
        check stmt.fcDecl.params[1].name == "b"
        check stmt.fcDecl.params[1].typ.kind == exprIdent
        check stmt.fcDecl.params[1].typ.ident == "int"
        check stmt.fcDecl.ret.isEmpty()
        check stmt.fcDecl.body.code.len == 0
      test "with parameters, with return":
        parser.lexer = initLexer("fc buz(foo: string): LinkedList[char] end")
        stmt = parser.parseStmt()
        check stmt.kind == stmtFcDecl
        check stmt.fcDecl.name == "buz"
        check stmt.fcDecl.params.len == 1
        check stmt.fcDecl.params[0].name == "foo"
        check stmt.fcDecl.params[0].typ.kind == exprIdent
        check stmt.fcDecl.params[0].typ.ident == "string"
        check stmt.fcDecl.ret.kind == exprOp
        check stmt.fcDecl.ret.op.kind == opIndex
        check stmt.fcDecl.ret.op.operands.len == 2
        check stmt.fcDecl.ret.op.operands[0].kind == exprIdent
        check stmt.fcDecl.ret.op.operands[0].ident == "LinkedList"
        check stmt.fcDecl.ret.op.operands[1].kind == exprIdent
        check stmt.fcDecl.ret.op.operands[1].ident == "char"
        check stmt.fcDecl.body.code.len == 0
      test "with body":
        parser.lexer = initLexer("""
        fc addi(a: int, b: int): int
           ret a + b;
        end
        """)
        stmt = parser.parseStmt()
        check stmt.kind == stmtFcDecl
        check stmt.fcDecl.name == "addi"
        check stmt.fcDecl.params.len == 2
        check stmt.fcDecl.params[0].name == "a"
        check stmt.fcDecl.params[0].typ.kind == exprIdent
        check stmt.fcDecl.params[0].typ.ident == "int"
        check stmt.fcDecl.params[1].name == "b"
        check stmt.fcDecl.params[1].typ.kind == exprIdent
        check stmt.fcDecl.params[1].typ.ident == "int"
        check stmt.fcDecl.ret.kind == exprIdent
        check stmt.fcDecl.ret.ident == "int"
        check stmt.fcDecl.body.code.len == 1
        check stmt.fcDecl.body.code[0].kind == stmtRet
        check stmt.fcDecl.body.code[0].ret.kind == exprOp
        check stmt.fcDecl.body.code[0].ret.op.kind == opPlus
        check stmt.fcDecl.body.code[0].ret.op.operands.len == 2
        check stmt.fcDecl.body.code[0].ret.op.operands[0].kind == exprIdent
        check stmt.fcDecl.body.code[0].ret.op.operands[0].ident == "a"
        check stmt.fcDecl.body.code[0].ret.op.operands[1].kind == exprIdent
        check stmt.fcDecl.body.code[0].ret.op.operands[1].ident == "b"
