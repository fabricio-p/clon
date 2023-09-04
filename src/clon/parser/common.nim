import ../[lexer, ast, box, util] # , error_reporter, env
from strutils import HexDigits, join, `%`
import strformat, sequtils
# nimble libs
import questionable/options
export lexer, ast, box, util

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

template at*(parser: Parser, offset: int): untyped = parser.lexer.src[offset]
template lookahead*(parser: Parser): Token = parser.lexer.token

proc fatalError*(lexer: typedesc[Lexer],
                pos: Pos,
                status: Status) {.noreturn.} =
  raise newException(
    Defect,
    fmt("Lexer error: {status.symbolName}\nAt: {pos.line}"))

template `!`*(tkResult: (Token, Status)): Token =
  let (token, status) = tkResult
  if status.isOk(): token
  else:
    fatalError(Lexer, token.span.e, status)

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

proc error*(parser: Parser, kind: ParserErrorKind, args: varargs[string]) =
  let pos = (parser.lookahead.span.s.line,
             parser.getColumn(parser.lookahead.span.s))
  raise ParserError.new(kind, pos, args)

proc expect*(parser: var Parser,
            kinds: set[TokenKind],
            consume: static[bool] = true): Token =
  if parser.lookahead.kind notin kinds:
    let kind = parser.lookahead.kind
    parser.error(
      peExpectedVsFound,
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

proc eat*(parser: var Parser, kinds: set[TokenKind]): TokenKind =
  if parser.lookahead.kind.withas(kind) in kinds:
    discard parser.lexer.next()
    result = kind
  else:
    result = tokNone

proc expect*(parser: var Parser,
            kind: TokenKind,
            consume: static[bool] = true): Token =
  parser.expect({ kind }, consume)

proc eat*(parser: var Parser, kind: TokenKind): TokenKind =
  parser.eat({ kind })

proc missingType*(parser: Parser) = parser.error(peMissingType)

template `?`*(tokenKind: TokenKind): bool = tokenKind != tokNone

func toPrefOp*(kind: TokenKind): OpKind {.inline.} =
  case kind
  of tokMinus: opMinus
  of tokNot: opNot
  else: opNone

func toInOp*(kind: TokenKind): OpKind {.inline.} =
  if kind == tokLBracket: opIndex
  else                  : OpKind(ord(kind) - ord(tokPlus) + 1)

func toPostOp*(kind: TokenKind): OpKind {.inline.} = opNone
