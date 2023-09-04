import strformat
# nimble libs
import questionable/options
# project modules
import ./parser/[common, expressions]
export Parser, ParserError, ParserErrorKind

# NOTE: It'd be nice to have a dependency graph of the parsing functions
# NOTE: Maybe add `::` operator for namespacing and such

###############################################################################
#---------------------------- Expression parsing ----------------------------##
###############################################################################

type Precedence = range[-1..20]

using
  parser: var Parser

let
  precInfix: array[OpKind, tuple[left, right: Precedence]] = static:
    var precs: array[OpKind, (Precedence, Precedence)]
    let mapping = {
      opNone:     (-1, -1),
      opAssign:   (14,  1),
      opOr:       ( 2,  3),
      opAnd:      ( 4,  5),
      opEq:       ( 6,  7),
      opGt:       ( 8,  9),
      opLt:       ( 8,  9),
      opGe:       ( 8,  9),
      opLe:       ( 8,  9),
      opPlus:     (10, 11),
      opMinus:    (10, 11),
      opAsterisk: (12, 13),
      opSlash:    (12, 13),
      # opArrow:    (15, 16),
      opDot:      (19, 20),
    }
    for (kind, prec) in mapping:
      precs[kind] = (Precedence(prec[0]), Precedence(prec[1]))
    precs
  precPrefix = Precedence(17)
  precPostfix = Precedence(18)

proc parseExpr*(parser;
                minPrec = Precedence.low,
                stopTokens: set[TokenKind] = {}): Expr
proc parseBlock*(parser; termins: set[TokenKind] = {}): Block[Stmt]

proc parseField(parser; typeRequired: static[bool] = true): Field =
  let tkName = parser.expect(tokIdent)
  discard parser.expect(tokColon)
  result.name = parser.lexer[tkName.span]
  result.typ =
    when not typeRequired:
      parser.parseExpr(stopTokens = { tokAssign, tokComma })
    else:
      parser.parseExpr(stopTokens = { tokAssign })
  when typeRequired:
    if result.typ.kind == exprNone: parser.missingType()

proc parseFcBlock[TF: FcExpr|FcDecl](parser): TF =
  when TF is FcDecl:
    result.name = parser.lexer[parser.expect(tokIdent).span]
    discard parser.expect(tokLParen)
  if parser.lookahead.kind != tokRParen: result.params = @[]
  while not ?parser.eat({tokRParen, tokEof}):
    let arg = parser.parseField(TF is FcDecl)
    # stdout.print(arg.typ)
    result.params.add(arg)
    if not ?parser.eat(tokComma):
      discard parser.expect(tokRParen, consume = false)
  if ?parser.eat(tokColon):
    result.ret = box(parser.parseExpr())
  result.body = parser.parseBlock()
  discard parser.expect(tokEnd)

proc parseFcType(parser): FcType =
  ## Parses function types, continuing from the state where the opening
  ## parenthesis that indicates the list of the argument (types) is the current
  ## token and ':' is the lookahead
  ##
  ## EBNF specification:
  ## .. code-block:: 
  ##
  ##    fctype := 'fc' '(' fctype_args ')'
  ##    fctype_args := ':' {type {',' fctype}}
  ##
  ## Examples:
  ## .. code-block::
  ##
  ##    fc(:) # no arguments, no returns
  ##    fc(: int) # receives an int, no returns
  ##    fc(:): string # no arguments, returns string
  ##    fc(: int, int): int # receives 2 ints, returns int
  discard parser.lexer.next() # tokColon
  if ?parser.eat(tokRParen):
    result.params = @[]
  else: # we have argument types
    while not ?parser.eat(tokRParen):
      result.params.add(parser.parseExpr())
      if result.params[^1].kind == exprNone: parser.missingType()
      if ?parser.eat(tokComma):
        # NOTE: Maybe remove the colons except the first one that comes
        #       directly after the opening parenthesis, because is required
        #       to make the distinction between between `FcExpr` and `FcType`.
        #
        # we are forced to check and skip the colons after we encounter and
        # skip a comma, because we skip the first colon at the top, so we can't
        # skip the colon first and then proceed to parse the type.
        discard parser.expect(tokColon)
      else:
        discard parser.expect(tokRParen, consume = false)
    if ?parser.eat(tokColon):
      result.ret = box(parser.parseExpr())

proc bracketExpr(parser): seq[Expr] =
  discard parser.lexer.next()
  if not ?parser.eat(tokRBracket):
    result = @[]
    while not ?parser.eat(tokRBracket):
      let item = parser.parseExpr()
      if item.kind == exprNone:
        parser.error(peExpected, "valid expression")
      result.add(item)
      if not ?parser.eat(tokComma):
        discard parser.expect(tokRBracket, consume = false)

proc boxValExpr(parser): seq[(?string, Expr)] =
  discard parser.lexer.next()
  if not ?parser.eat(tokRBrace):
    result = @[]
    while not ?parser.eat(tokRBrace):
      let key =
        if ?parser.eat(tokDot):
          let keyName = parser.lexer[parser.expect(tokIdent).span]
          discard parser.expect(tokAssign)
          some(keyName)
        else:
          none(string)
      let value = parser.parseExpr()
      if value.kind == exprNone:
        parser.error(peExpected, "valid expression")
      result.add((key, value))
      if not ?parser.eat(tokComma):
        discard parser.expect(tokRBrace, consume = false)

proc primaryExpr(parser; stopTokens: set[TokenKind] = {}): Expr =
  let stopTokens = stopTokens + {
    tokEof,
    tokEnd,
    tokSemicolon,
    tokRParen,
    tokRBracket,
    tokRBrace
  }
  let kind = parser.lookahead.kind
  if kind in stopTokens:
    return Expr(kind: exprNone)
  case kind
  of tokIntLit:
    result = Expr(kind: exprLit, lit: Lit(kind: litInt, i: parser.intExpr()))
  of tokFloatLit:
    result = Expr(kind: exprLit,
                  lit: Lit(kind: litFloat, f: parser.floatExpr()))
  of tokChrLit:
    if charc =? parser.chrExpr():
      result = Expr(kind: exprLit, lit: Lit(kind: litChr, c: charc))
    else:
      # TODO: make error kind for this
      doAssert false, "Invalid character"
  of tokStrLit:
    result = Expr(kind: exprLit, lit: Lit(kind: litStr, s: parser.strExpr()))
  of tokIdent:
    result = Expr(kind: exprIdent,
                  ident: parser.lexer[(!parser.lexer.next()).span])
  of tokLParen:
    discard parser.lexer.next()
    result = parser.parseExpr(0)
    discard parser.expect(tokRParen)
  of tokLBracket:
    result = Expr(kind: exprBracket, bracket: parser.bracketExpr())
  of tokLBrace:
    result = Expr(kind: exprBoxVal, boxVal: parser.boxValExpr())
  of tokFc:
    # In the expression/statement main parsing function, skip `fc`. If there is
    # an identifier, `parseFcBlock[FcDecl]`. The function that does that
    # continues from that point. Otherwise, skip the open parenthesis. Then if
    # there is a colon `parseFcType`, otherwise `parseFcBlock[FcDecl]`. Both
    # these functions should be able to parse from that point on.
    discard parser.lexer.next() # 'fc'
    discard parser.expect(tokLParen)
    if parser.lookahead.kind == tokColon:
      result = Expr(kind: exprFcType, fcType: parser.parseFcType())
    else:
      result = Expr(kind: exprFc, fc: parser.parseFcBlock[:FcExpr]())
  of tokEof:
    result = Expr(kind: exprNone)
  of InfixOpTokens:
    parser.error(peUnexpected, fmt"""infix operator "{kind}"""")
  else:
    echo parser.lookahead
    todo("Either invalid or not-yet-handled token")

proc fcArgs(parser): seq[Expr] =
  while not ?parser.eat(tokRParen):
    result.add(parser.parseExpr(0))
    if not ?parser.eat(tokComma):
      discard parser.expect(tokRParen, consume = false)

proc parseFcCall(parser; callee: sink Expr): Expr =
  discard parser.lexer.next() # the checking for the opening parenthesis was
                              # made at the level calling this
  result = Expr(kind: exprFcCall,
                fcCall: FcCall(callee: box(callee), args: parser.fcArgs()))

proc parseExpr(parser;
               minPrec = Precedence.low,
               stopTokens: set[TokenKind] = {}) : Expr =
  let stopTokens = stopTokens + {
    tokEof,
    tokEnd,
    tokSemicolon,
    tokRParen,
    tokRBracket,
    tokRBrace
  }
  result =
    if parser.lookahead.kind.withas(pref) in PrefixOpTokens:
      discard parser.lexer.next()
      Expr(kind: exprOp,
           op: Op(kind: pref.toPrefOp(),
                  operands: @[parser.parseExpr(precPrefix, stopTokens)]))
    else:
      parser.primaryExpr(stopTokens)
  while parser.lookahead.kind.withas(afterKind) notin stopTokens:
    if afterKind == tokEOF: break
    elif afterKind == tokLBracket: # this is a special case
      if precPostfix < minPrec: break
      discard parser.lexer.next()
      if result.kind != exprOp or result.op.kind != opIndex:
        result = Expr(kind: exprOp, op: Op(kind: opIndex, operands: @[result]))
      result.op.operands.add(parser.parseExpr())
      discard parser.expect(tokRBracket)
      continue
    elif afterKind in PostfixOpTokens:
      if precPostfix < minPrec: break
      let opKind = afterKind.toPostOp()
      result = Expr(kind: exprOp, op: Op(kind: opKind, operands: @[result]))
      discard parser.lexer.next()
      continue
    if afterKind == tokLParen:
      result = parser.parseFcCall(result)
      continue
    if afterKind in InfixOpTokens:
      let
        opKind = afterKind.toInOp()
        prec = precInfix[opKind]
      if prec.left < minPrec: break
      discard parser.lexer.next()
      let rhs = parser.parseExpr(prec.right)
      if result.kind == exprOp and result.op.kind == opKind:
        result.op.operands.add(rhs)
      else:
        result = Expr(kind: exprOp,
                      op: Op(kind: opKind, operands: @[result, rhs]))
      continue
    break

###############################################################################
#----------------------------- Statement parsing -----------------------------#
###############################################################################

proc parseLoc(parser): VarDecl =
  discard parser.lexer.next()
  let field = parser.parseField()
  result.name = field.name
  result.typ = field.typ
  if ?parser.eat(tokAssign):
    result.value = parser.parseExpr()
    if result.value.kind == exprNone: parser.error(peExpected, "expression")
  discard parser.expect(tokSemicolon)

proc parseIf(parser): IfStmt =
  # The structure of the if statemet is as follows
  # if
  # ?(cond1)
  #   ... block1 ...
  # ?(cond2)
  #   ... block2 ...
  # .
  # .
  # .
  # ?(condn)
  #   ... blockn ...
  # ?()
  #   ... block else...
  # end
  # NOTE: The else clause should be the last one
  var elseReached = false
  discard parser.lexer.next()
  while not ?parser.eat(tokEnd):
    if elseReached:
      parser.error(peElseNotLast)
    var clause: IfClause
    discard parser.expect(tokQuestion)
    discard parser.expect(tokLParen)
    # TODO: Rethink this
    if parser.lookahead.kind == tokRParen: elseReached = true
    else: clause.cond = parser.parseExpr()
    discard parser.expect(tokRParen)
    clause.body = parser.parseBlock({tokQuestion})
    result.add(clause)

proc parseStmt*(parser): Stmt

proc parseForLoop(parser): ForLoop =
  if not ?parser.eat(tokSemicolon):
    result.init = box(parser.parseStmt())
    if result.init.kind notin {stmtVarDecl, stmtExpr, stmtRet, stmtNone}:
      discard parser.expect(tokComma)
  result.cond = parser.parseExpr()
  discard parser.expect(tokSemicolon)
  result.step = parser.parseExpr()
  discard parser.expect(tokRParen)
  result.body = parser.parseBlock()
  discard parser.expect(tokEnd)

proc parseFor(parser): Stmt =
  # for loops:
  # either
  # for (a in b) # no loc
  #   ... body ...
  # end
  # or
  # for (loc i = 0; i < j; i = i + 1) # with loc
  #   ... body ...
  # end
  discard parser.lexer.next()
  discard parser.expect(tokLParen)
  if parser.lookahead.kind == tokIdent:
    let
      preIdentPos = parser.lexer.pos
      identToken = !parser.lexer.next()
      ident = parser.lexer[identToken.span]
    if ?parser.eat(tokIn):
      let iter = parser.parseExpr()
      discard parser.expect(tokRParen)
      let body = parser.parseBlock()
      discard parser.expect(tokEnd)
      result = Stmt(kind: stmtForInLoop,
                    forinl: ForInLoop(capture: ident, iter: iter, body: body))
    else:
      # a kind of a dirty way of >1 lookaheading
      parser.lexer.revert(identToken, preIdentPos)
      result = Stmt(kind: stmtForLoop, forl: parser.parseForLoop())
  else:
    result = Stmt(kind: stmtForLoop, forl: parser.parseForLoop())

proc parseFcExpr(parser): Expr {.inline.} =
  # 'fc' has been skiped by a previous function higher up the call stack
  discard parser.expect(tokLParen)
  if parser.lookahead.kind == tokColon:
    result = Expr(kind: exprFcType, fcType: parser.parseFcType())
  else:
    result = Expr(kind: exprFc, fc: parser.parseFcBlock[:FcExpr]())

proc parseStmt*(parser): Stmt =
  let start = parser.lookahead.span.s
  case parser.lookahead.kind
  of tokIf:
    result = Stmt(kind: stmtIf, ifs: parser.parseIf())
  of tokFor:
    result = parser.parseFor()
  of tokLoc:
    result = Stmt(kind: stmtVarDecl, varDecl: parser.parseLoc())
  of tokWhl:
    discard parser.lexer.next()
    discard parser.expect(tokLParen)
    if parser.lookahead.kind == tokRParen:
      parser.error(peExpected, "condition expression", fmt""""{tokRParen}"""")
    let cond = parser.parseExpr()
    discard parser.expect(tokRParen)
    let body = parser.parseBlock()
    discard parser.expect(tokEnd)
    result = Stmt(kind: stmtWhlLoop, whll: WhlLoop(cond: cond, body: body))
  of tokRet:
    result = Stmt(kind: stmtRet)
    discard parser.lexer.next()
    if parser.lookahead.kind != tokSemicolon:
      let expr = parser.parseExpr()
      discard parser.expect(tokSemicolon)
      result.ret = expr
    else:
      result.ret = Expr(kind: exprNone)
      discard parser.lexer.next()
  of tokFc:
    discard parser.lexer.next() # 'fc'
    if parser.lookahead.kind == tokIdent:
      result = Stmt(kind: stmtFcDecl, fcDecl: parser.parseFcBlock[:FcDecl]())
    else:
      result = Stmt(kind: stmtExpr, expr: parser.parseFcExpr())
      discard parser.expect(tokSemicolon)
  of tokSemicolon:
    discard parser.lexer.next()
    result = Stmt(kind: stmtNone)
  else:
    result = Stmt(kind: stmtExpr, expr: parser.parseExpr())
    discard parser.expect(tokSemicolon)
  let `end` = parser.lexer.pos
  result.span = (start, `end`)

proc parseBlock*(parser; termins: set[TokenKind] = {}): Block[Stmt] =
  let stopTokens = termins + {tokEnd, tokEof}
  result = initBlock[Stmt]()
  while parser.lookahead.kind notin stopTokens:
    let stmt = parser.parseStmt()
    if stmt.kind != stmtNone: result.code.add(stmt)

###############################################################################
#----------------------------- Top level parsing -----------------------------#
###############################################################################

proc parseBoxDecl(parser): BoxDecl = todo("Box declaration")

proc parseTopLevel(parser): TopLevel =
  let start = parser.lookahead.span.s
  case parser.lookahead.kind
  of tokFc:
    todo("Top level function declaration")
  of tokBox:
    todo("Top level box declaration")
  of tokLoc:
    todo("Top level module variable")
  else:
    todo("Other top level construct or semantic(?) error")
  let `end` = parser.lexer.pos
  result.span = (start, `end`)
