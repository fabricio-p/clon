# stdlib
from strutils import HexDigits
from parseutils import parseHex,
                       parseOct,
                       parseBin,
                       parseBiggestInt,
                       parseFloat
import strformat, terminal, std/enumutils
# nimble libs
import questionable/options
# project modules
import ./lexer, ./ast, ./box, ./util # , ./error_reporter, ./env

type
  Parser* = object
    lexer*: Lexer
    # ast*: Ast
    # rep*: ErrReporter

template todo(msg: static[string] = ""): untyped =
  let (file, line, _) = instantiationInfo()
  stderr.styledWriteLine(
    fgCyan, "Code path not yet implemented (", msg, ") ",
    fgWhite, "[",
    fgGreen, file,
    fgWhite, ":",
    fgGreen, $line,
    fgWhite, "]")
  quit(QuitFailure)

template at(parser: Parser, offset: int): untyped = parser.lexer.src[offset]
template lookahead(parser: Parser): Token = parser.lexer.token

proc fatalError(lexer: typedesc[Lexer], pos: Pos,
                status: Status) {.noreturn.} =
  raise newException(Defect):
          "Lexer error: {status.symbolName}\nAt: {pos.line}".fmt

template `!`(tkResult: (Token, Status)): Token =
  let (token, status) = tkResult
  if status.isOk(): token
  else:
    fatalError(Lexer, token.span.e, status)

template `=!`(sym: untyped, tkResult: (Token, Status)): bool =
  (var `sym` = tkResult[0]; tkResult[1])

func biggestInt(s: openArray[char], n: var BiggestInt, maxLen: int): int =
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
      opOr:       ( 1,  2),
      opAnd:      ( 3,  4),
      opEq:       ( 5,  6),
      opGt:       ( 7,  8),
      opLt:       ( 7,  8),
      opGe:       ( 7,  8),
      opLe:       ( 7,  8),
      opPlus:     ( 9, 10),
      opMinus:    ( 9, 10),
      opAsterisk: (11, 12),
      opSlash:    (11, 12),
      opAssign:   (14, 13),
      opDot:      (20, 19)
    }
    for (kind, prec) in mapping:
      precs[kind] = (Precedence(prec[0]), Precedence(prec[1]))
    precs
  precPrefix = Precedence(17)
  precPostfix = Precedence(18)

proc parseExpr*(parser: var Parser, minPrec: int8 = 0): Expr

# 
# proc parseFcCallArgs(parser: var Parser, env: var Env): seq[Expr] =
#   while not parser.lexer.token.kind == tokComma:
#     discard parser.lexer.next() # the first time it skips '(' and then it keeps
#                              # skipping the commas
#     result.add(parseExpr(parser))
#   if parser.lexer.token.kind != tokRParen:
#     discard

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
      doAssert false, "Invalid character" # TODO: Report, detailed
  of tokStrLit:
    result = Expr(kind: exprLit, lit: Lit(kind: litStr, s: parser.strExpr()))
  of tokIdent:
    result = Expr(kind: exprIdent,
                  ident: parser.lexer[(!parser.lexer.next()).span])
  of tokLParen:
    discard parser.lexer.next()
    result = parser.parseExpr(0)
    doAssert (!parser.lexer.next()).kind == tokRParen
  of InfixOpTokens:
    quit("Expected expression, found infix operator", QuitFailure)
  else:
    todo("Token doesn't belong in an expression")

proc fcArgs(parser: var Parser): seq[Expr] =
  while parser.lookahead.kind != tokRParen:
    discard parser.lexer.next()
    result.add(parser.parseExpr(0))
    if parser.lookahead.kind notin {tokComma, tokRParen}:
      quit("Expected comma or closing parenthesis after argument", QuitFailure)

proc parseFc(parser: var Parser, callee: sink Expr): Expr =
  result = Expr(kind: exprFcCall,
                fcCall: FcCall(callee: box(callee), args: parser.fcArgs()))
  doAssert (!parser.lexer.next()).kind == tokRParen:
    "Expected closing parenthesis (function call)" # TODO: Report, detailed

func toPrefOp(kind: TokenKind): OpKind {.inline.} =
  case kind
  of tokMinus: opMinus
  of tokNot: opNot
  else: opNone

func toInOp(kind: TokenKind): OpKind {.inline.} =
  if kind == tokLBracket: opIndex
  else                  : OpKind(int(kind) - int(tokPlus) + 1)

func toPostOp(kind: TokenKind): OpKind {.inline.} =
  opNone

proc parseExpr(parser: var Parser, minPrec: int8 = 0): Expr =
  result =
    if parser.lookahead.kind.withas(pref) in PrefixOpTokens:
      discard parser.lexer.next()
      Expr(kind: exprOp,
           op: Op(kind: pref.toPrefOp(),
                  operands: @[parser.parseExpr(precPrefix)]))
    else:
      parser.primaryExpr()
  forever:
    let afterKind = parser.lookahead.kind
    if afterKind == tokEOF: break
    elif afterKind in PostfixOpTokens:
      if precPostfix < minPrec: break
      let opKind = afterKind.toPostOp()
      result = Expr(kind: exprOp, op: Op(kind: opKind, operands: @[result]))
      continue
    if afterKind == tokLParen:
      result = parser.parseFc(result)
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
      if result.op.kind == opIndex:
        doAssert (!parser.lexer.next()).kind == tokRBracket:
          "Expected closing bracket" # TODO: Error report, detailed
      continue
    break
