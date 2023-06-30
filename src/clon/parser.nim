# stdlib
from strutils import HexDigits, join, `%`
from parseutils import parseHex,
                       parseOct,
                       parseBin,
                       parseBiggestInt,
                       parseFloat
import strformat, terminal, sequtils, std/enumutils
# nimble libs
import questionable/options
# project modules
import ./lexer, ./ast, ./box, ./util # , ./error_reporter, ./env


# TODO: Make better type parsing
# NOTE: Maybe add `::` operator for namespacing and such

type
  Parser* = object
    lexer*: Lexer
    # ast*: Ast
    # rep*: ErrReporter
  ParserError* = object of CatchableError
    # src: string
    # span: Span
  ParserErrorKind* = enum
    peExpected = "Expected $#"
    peExpectedVsFound = "Expected $#, found $#"
    peUnexpected = "Unexpected $#"
    peMissingType = "Type specification missing"
    peElseNotLast = "The '?()' (aka. else) clause should be the last one"
    peDuplicateElse = "Duplicate '?()' (aka. else) clause encountered"

template at(parser: Parser, offset: int): untyped = parser.lexer.src[offset]
template lookahead(parser: Parser): Token = parser.lexer.token

proc fatalError(lexer: typedesc[Lexer],
                pos: Pos,
                status: Status) {.noreturn.} =
  raise newException(
    Defect,
    fmt("Lexer error: {status.symbolName}\nAt: {pos.line}"))

template `!`(tkResult: (Token, Status)): Token =
  let (token, status) = tkResult
  if status.isOk(): token
  else:
    fatalError(Lexer, token.span.e, status)

# template `=!`(sym: untyped, tkResult: (Token, Status)): bool =
#   (var `sym` = tkResult[0]; tkResult[1])

func new(_: typedesc[ParserError],
         kind: ParserErrorKind,
         pos: (int, int),
         args: varargs[string]): ref ParserError =
  let
    posText = fmt", at {pos[0]}:{pos[1]}"
    message = ($kind % args) & posText
  result = newException(ParserError, message)

func getColumn(parser: Parser, pos: Pos): int =
  result = 0
  var offset = pos.offset - 1
  while offset >= 0 and parser.lexer.src[offset] != '\n':
    dec offset
    inc result

proc error(parser: Parser, kind: ParserErrorKind, args: varargs[string]) =
  let pos = (parser.lookahead.span.s.line,
             parser.getColumn(parser.lookahead.span.s))
  raise ParserError.new(kind, pos, args)

proc expect(parser: var Parser,
            kinds: set[TokenKind],
            consume: static[bool] = true): Token =
  if parser.lookahead.kind notin kinds:
    let kind = parser.lookahead.kind
    parser.error(peExpectedVsFound,
                 "any of the tokens in the list [" &
                  kinds
                    .mapIt(if it in {tokIntLit..tokIdent, tokEof}: $it
                           else: "\"" & $it & "\"")
                    .join(",") &
                  "]",
                 if kind in {tokIntLit..tokIdent, tokEof}:
                   fmt"""{kind} "{parser.lexer[parser.lookahead.span]}""""
                 else:
                   "\"" & $parser.lookahead.kind & "\"")
  elif consume:
    result = !parser.lexer.next()
  else:
    result = parser.lookahead

proc eat(parser: var Parser, kinds: set[TokenKind]): TokenKind =
  if parser.lookahead.kind.withas(kind) in kinds:
    discard parser.lexer.next()
    result = kind
  else:
    result = tokNone

proc expect(parser: var Parser,
            kind: TokenKind,
            consume: static[bool] = true): Token =
  parser.expect({ kind }, consume)

proc eat(parser: var Parser, kind: TokenKind): TokenKind =
  parser.eat({ kind })

proc missingType(parser: Parser) = parser.error(peMissingType)

template `?`(tokenKind: TokenKind): bool = tokenKind != tokNone

###############################################################################
#---------------------------- Expression parsing ----------------------------##
###############################################################################

func biggestInt(s: openArray[char], n: var BiggestInt, _: int): int =
  s.parseBiggestInt(n)

proc intExpr(parser: var Parser): BiggestInt =
  template fn(n: untyped): untyped =
    proc(a: openArray[char], b: var BiggestInt, c: int): int =
      `parse n`[BiggestInt](a, b, c)
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset
    finish = tk.span.e.offset
    parseFn = if tk.span.e.offset - start >= 3 and parser.at(start) == '0':
                case parser.at(start + 1)
                of 'x', 'X': fn(Hex) # parseHex[BiggestInt]
                of 'o', 'O': fn(Oct) # parseOct[BiggestInt]
                of 'b', 'B': fn(Bin) # parseBin[BiggestInt]
                else: biggestInt
              else: biggestInt
  discard parseFn(parser.lexer.src.toOpenArray(start, finish - 1),
                  result, 0)

proc floatExpr(parser: var Parser): float =
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset
  discard parseFloat(parser.lexer.src, result, start)

proc escSeq(parser: var Parser, start: int): (?char, int) =
  var offset = start
  result[0] =
    case parser.at(offset)
    of 'r': some('\r')
    of 'n': some('\n')
    of 'e': some('\x1f')
    of 't': some('\t')
    of '\\': some('\\')
    of '0': some('\0')
    of 'x', 'X':
      if offset + 2 < parser.lexer.src.len and
        parser.at(offset + 1) in HexDigits and
        parser.at(offset + 2) in HexDigits:
        var hex: int
        discard parser.lexer.src.toOpenArray(offset + 1, offset + 2)
                                .parseHex(hex)
        inc offset, 2
        hex.char.some
      else: char.none
    else: char.none
  inc offset
  result[1] = offset

proc chrExpr(parser: var Parser): ?char =
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset + 1
  if parser.at(start) == '\\':
    if parser.at(start + 1) == '\'': some('\'')
    else:
      escSeq(parser, start + 1)[0]
  else:
    some(parser.at(start))

proc strExpr(parser: var Parser): string =
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset
    len = tk.span.e.offset - start
  result = newStringOfCap(len)
  var
    i = start + 1
    item: char
  while i < tk.span.e.offset - 1:
    if parser.at(i) == '\\':
      inc i
      let cc = parser.at(i)
      case cc
      of '"': item = '"'
      of 'r': item = '\r'
      of 't': item = '\t'
      of 'n': item = '\n'
      of 'e': item = '\e'
      of '\\': item = '\\'
      of 'x', 'X':
        inc i
        if parser.lexer.src.len > i + 2:
          if parser.at(i) notin HexDigits or
          parser.at(i + 1) notin HexDigits:
            # TODO !IMPORTANT: use the error throwing system
            quit("Invalid hex escape character", QuitFailure)
          else:
            var hex: int
            discard parser.lexer.src.toOpenArray(i, i + 1).parseHex(hex)
            inc i
            item = char(hex)
        else:
          quit("Invalid hex escape character", QuitFailure)
      else:
        quit("Invalid escape character", QuitFailure)
    else:
      item = parser.at(i)
    inc i
    result.add(item)

type Precedence = range[-1..20]

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
      opDot:      (19, 20),
    }
    for (kind, prec) in mapping:
      precs[kind] = (Precedence(prec[0]), Precedence(prec[1]))
    precs
  precPrefix = Precedence(17)
  precPostfix = Precedence(18)

proc parseExpr*(parser: var Parser,
                minPrec = Precedence.low,
                terminalTokens: set[TokenKind] = {}): Expr
proc parseBlock*(parser: var Parser, termins: set[TokenKind] = {}): Block[Stmt]

proc parseField(parser: var Parser): Field =
  let tkName = parser.expect(tokIdent)
  discard parser.expect(tokColon)
  result.name = parser.lexer[tkName.span]
  result.typ = parser.parseExpr(terminalTokens = { tokAssign })
  if result.typ.kind == exprNone: parser.missingType()

proc parseFcBlock[TF: FcExpr or FcDecl](parser: var Parser): TF =
  discard parser.lexer.next()
  when TF is FcDecl: result.name = parser.lexer[parser.expect(tokIdent).span]
  discard parser.expect(tokLParen)
  if parser.lookahead.kind != tokRParen: result.params = @[]
  while not ?parser.eat({tokRParen, tokEof}):
    let arg = parser.parseField()
    result.params.add(arg)
    if not ?parser.eat(tokComma):
      discard parser.expect(tokRParen, consume = false)
  if ?parser.eat(tokColon):
    result.ret = box(parser.parseExpr())
  result.body = parser.parseBlock()
  discard parser.expect(tokEnd)

proc bracketExpr(parser: var Parser): seq[Expr] =
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

proc primaryExpr(parser: var Parser): Expr =
  let kind = parser.lookahead.kind
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
  of tokFc:
    result = Expr(kind: exprFc, fc: parser.parseFcBlock[:FcExpr]())
  of tokEof:
    result = Expr(kind: exprNone)
  of InfixOpTokens:
    parser.error(peUnexpected, fmt"""infix operator "{kind}"""")
  else:
    echo parser.lookahead
    todo("Either invalid or not-yet-handled token")

proc fcArgs(parser: var Parser): seq[Expr] =
  while not ?parser.eat(tokRParen):
    result.add(parser.parseExpr(0))
    if not ?parser.eat(tokComma):
      discard parser.expect(tokRParen, consume = false)

proc parseFcCall(parser: var Parser, callee: sink Expr): Expr =
  discard parser.lexer.next() # the checking for the opening parenthesis was
                              # made at the level calling this
  result = Expr(kind: exprFcCall,
                fcCall: FcCall(callee: box(callee), args: parser.fcArgs()))

func toPrefOp(kind: TokenKind): OpKind {.inline.} =
  case kind
  of tokMinus: opMinus
  of tokNot: opNot
  else: opNone

func toInOp(kind: TokenKind): OpKind {.inline.} =
  if kind == tokLBracket: opIndex
  else                  : OpKind(ord(kind) - ord(tokPlus) + 1)

func toPostOp(kind: TokenKind): OpKind {.inline.} = opNone

proc parseExpr(parser: var Parser,
               minPrec = Precedence.low,
               terminalTokens: set[TokenKind] = {}) : Expr =
  let terminalTokens = terminalTokens + {
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
                  operands: @[parser.parseExpr(precPrefix, terminalTokens)]))
    else:
      parser.primaryExpr()
  while parser.lookahead.kind.withas(afterKind) notin terminalTokens:
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

proc parseLoc(parser: var Parser): VarDecl =
  discard parser.lexer.next()
  let field = parser.parseField()
  result.name = field.name
  result.typ = field.typ
  if ?parser.eat(tokAssign):
    result.value = parser.parseExpr()
    if result.value.kind == exprNone: parser.error(peExpected, "expression")
  discard parser.expect(tokSemicolon)

proc parseIf(parser: var Parser): IfStmt =
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
    if parser.lookahead.kind == tokRParen: elseReached = true
    else: clause.cond = parser.parseExpr()
    discard parser.expect(tokRParen)
    clause.body = parser.parseBlock({tokQuestion})
    result.add(clause)

proc parseStmt*(parser: var Parser): Stmt

proc parseForLoop(parser: var Parser): ForLoop =
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

proc parseFor(parser: var Parser): Stmt =
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

proc parseStmt*(parser: var Parser): Stmt =
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
    result = Stmt(kind: stmtFcDecl, fcDecl: parser.parseFcBlock[:FcDecl]())
  of tokSemicolon:
    discard parser.lexer.next()
    result = Stmt(kind: stmtNone)
  else:
    result = Stmt(kind: stmtExpr, expr: parser.parseExpr())
    discard parser.expect(tokSemicolon)

proc parseBlock*(parser: var Parser,
                 termins: set[TokenKind] = {}): Block[Stmt] =
  let terminalTokens = termins + {tokEnd, tokEof}
  result = initBlock[Stmt]()
  while parser.lookahead.kind notin terminalTokens:
    let stmt = parser.parseStmt()
    if stmt.kind != stmtNone: result.code.add(stmt)
