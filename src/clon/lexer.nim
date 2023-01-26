import tables, strutils
import questionable/options
import ./status

type
  TokenKind* = enum
    tokEOF
    tokIntLit
    tokFloatLit
    tokStrLit
    tokChrLit
    tokIdent
    tokFc
    tokLoc
    tokIf
    tokWhl
    tokFor
    tokCase
    tokBox
    tokRet
    tokEnd
    tokLParen
    tokRParen
    tokLBracket
    tokRBracket
    tokLBrace
    tokRBrace
    tokPlus
    tokMinus
    tokAsterisk
    tokSlash
    tokAnd
    tokOr
    tokNot
    tokDot
    tokComma
    tokColon
    tokSemiColon
  Pos* = tuple[offset, line: int]
  Span* = tuple[s, e: Pos]
  Token* = object
    span*: Span
    kind*: TokenKind
  Lexer* = object
    src*: string
    pos*: Pos
    token*: Token
    status: Status

func empty(span: typedesc[Span]): Span = ((0, 1), (0, 1))

const
  keywords* = {
    "fc": tokFc,
    "loc": tokLoc,
    "if": tokIf,
    "whl": tokWhl,
    "for": tokFor,
    "case": tokCase,
    "box": tokBox,
    "ret": tokRet,
    "end": tokEnd,
    "and": tokAnd,
    "or": tokOr,
    "not": tokNot
  }.toTable
  eof* = char uint8.high
  EscapableChars* = {'r', 't', 'n', 'e', '\\'}

proc next*(lexer: var Lexer): (Token, Status)

func `[]`(lexer: Lexer, span: Span): string {.inline.} =
  lexer.src[span.s.offset..<span.e.offset]

proc initLexer*(src: string): Lexer =
  result.src = src
  result.pos = (0, 1)
  result.token = Token(kind: tokEOF, span: Span.empty)
  result.status = Ok
  discard result.next()

func skip(lexer: Lexer, chars: set[char],
          start = lexer.token.span.e): Pos =
  result = start
  while result.offset < lexer.src.len and lexer.src[result.offset] in chars:
    if lexer.src[result.offset] == '\n':
      inc result.line
    inc result.offset

func at(lexer: Lexer, offset: int): char {.inline.} =
  if offset < lexer.src.len: lexer.src[offset]
  else: eof

func current(lexer: Lexer): char {.inline.} =
  lexer.at(lexer.token.span.e.offset)

func peek(lexer: Lexer, offset: int = 1,
          start = lexer.token.span.e.offset): char {.inline.} =
  lexer.at(start + offset)

proc skipWhitespace(lexer: Lexer, start = lexer.token.span.e): Pos =
  lexer.skip(Whitespace)

template at(offset: int): char = lexer.src[offset]

proc lexIdent(lexer: var Lexer) =
  var offset = lexer.token.span.e.offset
  lexer.token.kind = tokIdent
  while offset < lexer.src.len and at(offset) in IdentChars: inc offset
  lexer.token.span.e.offset = offset

proc lexNumber(lexer: var Lexer) =
  lexer.token.kind = tokIntLit
  var
    offset = lexer.token.span.e.offset
    prevWasUnderscore = false
    base = 10
  template invalid() = lexer.status = InvalidNumLit
  defer: lexer.token.span.e.offset = offset
  if at(offset) in {'+', '-'}:
    inc offset
  if at(offset) == '0':
    inc offset
    if at(offset) in {'x', 'X', 'o', 'O', 'b', 'B'}:
      base = case at(offset)
               of 'x', 'X': 16
               of 'o', 'O': 8
               of 'b', 'B': 2
               else: 10
      inc offset
  while offset < lexer.src.len:
    let curr = at(offset)
    case curr
    of 'a'..'f', 'A'..'F':
      if base != 16:
        if base == 10 and (curr == 'e' or curr == 'E'): break
        else: invalid()
    of '8', '9':
      if base == 8 or base == 2: invalid()
    of '2'..'7':
      if base == 2: invalid()
    of '.':
      if prevWasUnderscore or base != 10: invalid()
      else: break
    of '_':
      if prevWasUnderscore: invalid()
      else: prevWasUnderscore = true
    else:
      if curr in 'f'.succ..'z' or curr in 'F'.succ..'Z': invalid()
      elif curr notin '0'..'9': break
    inc offset
  if lexer.at(offset) == '.':
    lexer.token.kind = tokFloatLit
    inc offset
    if lexer.at(offset) in '0'..'9':
      while offset < lexer.src.len and at(offset) in '0'..'9':
        inc offset
    else: invalid()
  if lexer.at(offset) == 'e' or lexer.at(offset) == 'E':
    inc offset
    if lexer.at(offset) == '+' or lexer.at(offset) == '-': inc offset
    if lexer.at(offset) notin '0'..'9': lexer.status = InvalidNumLit
    else:
      while offset < lexer.src.len and at(offset) in '0'..'9':
        inc offset
  if lexer.at(offset) in 'f'.succ..'z' or lexer.at(offset) in 'F'.succ..'Z':
    invalid()

proc lexStr(lexer: var Lexer) =
  var pos = lexer.token.span.e
  defer: lexer.token.span.e = pos
  lexer.token.kind = tokStrLit
  inc pos.offset
  while pos.offset < lexer.src.len and at(pos.offset) != '"':
    let curr = at(pos.offset)
    inc pos.offset
    if curr == '\\':
      if at(pos.offset) == 'x' or at(pos.offset) == 'X':
        inc pos.offset
        if lexer.src.len > pos.offset + 2:
          if (at(pos.offset) notin HexDigits or
            at(pos.offset + 1) notin HexDigits):
            lexer.status = InvalidEscSeq
            return
          else:
            pos.offset += 2
        else:
          break
    elif curr == '\n':
      inc pos.line
  if pos.offset < lexer.src.len and at(pos.offset) == '"':
    inc pos.offset
  else:
    lexer.status = UnterminatedStr

proc lexChar(lexer: var Lexer) =
  var
    pos = lexer.token.span.e
  inc pos.offset
  lexer.status =
    # get current char, inc offset, case on char
    case (let curr = at(pos.offset); inc pos.offset; curr)
    of '\n':
      if at(pos.offset) == '\'': CharNewLine
      else:                      UnterminatedChar
    of '\\':
      # get escaped char
      let curr = at(pos.offset)
      # inc offset
      inc pos.offset
      # compare char
      if curr == 'x' or curr == 'X':
        inc pos.offset
        if lexer.src.len > pos.offset + 2:
          if (at(pos.offset) notin HexDigits or
              at(pos.offset + 1) notin HexDigits):
            InvalidEscSeq
          else:
            inc pos.offset, 2
            Ok
        else:
          InvalidEscSeq
      elif curr in EscapableChars: Ok
      else: InvalidEscSeq
    else: Ok
  if lexer.status.isOk and lexer.at(pos.offset) != '\'':
    lexer.status = UnterminatedChar
  inc pos.offset
  lexer.token.span.e = pos
  lexer.token.kind = tokChrLit

proc next*(lexer: var Lexer): (Token, Status) =
  result = (lexer.token, lexer.status)
  lexer.pos = lexer.token.span.e
  if lexer.pos.offset >= lexer.src.high:
    lexer.token = Token(kind: tokEOF, span: (lexer.pos, lexer.pos))
    return
  lexer.token.span.s = lexer.skipWhitespace()
  lexer.token.span.e = lexer.token.span.s
  let ch = lexer.current
  case ch
  of '0'..'9': lexer.lexNumber()
  of '-', '+':
    if lexer.peek() in '0'..'9': lexer.lexNumber()
    else:
      inc lexer.token.span.e.offset
      lexer.token.kind = if ch == '-': tokMinus else: tokPlus
  of '"': lexer.lexStr()
  of 'a'..'z', 'A'..'Z', '_':
    lexer.lexIdent()
    lexer.token.kind = keywords.getOrDefault(lexer[lexer.token.span], tokIdent)
  of '\'': lexer.lexChar()
  else:
    inc lexer.token.span.e.offset
    lexer.token.kind = case ch
                         of '(': tokLParen
                         of ')': tokRParen
                         of '[': tokLBracket
                         of ']': tokRBracket
                         of '{': tokLBrace
                         of '}': tokRBrace
                         of '.': tokDot
                         of ',': tokComma
                         of ':': tokColon
                         of ';': tokSemicolon
                         of '*': tokAsterisk
                         of '/': tokSlash
                         else: tokEOF

iterator items*(lexer: var Lexer): Token =
  while lexer.token.kind != tokEOF and
    (let (t, s) = lexer.next(); s.isOk):
    yield t