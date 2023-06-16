import tables, strutils
import questionable/options

type
  Status* {.pure.} = enum
    Ok
    InvalidChar = "Invalid character"
    InvalidNumLit = "Invalid number literal"
    InvalidEscSeq = "Invalid escape sequence"
    UnterminatedStr = "Unterminated string character"
    UnterminatedChar = "Unterminated character literal"
    CharNewline
  TokenKind* = enum
    tokEOF       = "EOF"
    tokIntLit    = "$int"
    tokFloatLit  = "$float"
    tokStrLit    = "$str"
    tokChrLit    = "$char"
    tokIdent     = "$ident"
    tokFc        = "fc"
    tokLoc       = "loc"
    tokIf        = "if"
    tokWhl       = "whl"
    tokFor       = "for"
    tokIn        = "in"
    tokCase      = "case"
    tokBox       = "box"
    tokRet       = "ret"
    tokEnd       = "end"
    tokLParen    = "("
    tokRParen    = ")"
    tokLBracket  = "["
    tokRBracket  = "]"
    tokLBrace    = "{"
    tokRBrace    = "}"
    tokPlus      = "+"
    tokMinus     = "-"
    tokAsterisk  = "*"
    tokSlash     = "/"
    tokLt        = "<"
    tokAssign    = "="
    tokGt        = ">"
    tokLe        = "<="
    tokEq        = "=="
    tokGe        = ">="
    tokNot       = "not"
    tokNeq       = "!="
    tokAnd       = "and"
    tokOr        = "or"
    tokDot       = "."
    tokComma     = ","
    tokColon     = ":"
    tokQuestion  = "?"
    tokSemiColon = ";"
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

func isOk*(status: Status): bool {.inline.} = status == Status.Ok

func empty(span: typedesc[Span]): Span = ((0, 1), (0, 1))

const
  keywords* = {
    "fc": tokFc,
    "loc": tokLoc,
    "if": tokIf,
    "whl": tokWhl,
    "for": tokFor,
    "in": tokIn,
    "case": tokCase,
    "box": tokBox,
    "ret": tokRet,
    "end": tokEnd,
    "and": tokAnd,
    "or": tokOr,
    "not": tokNot
  }.toTable
  Eof* = cast[char](uint8.high)
  EscapableChars* = {'r', 't', 'n', 'e', '0', '\\'}
  InfixOpTokens* = {tokPlus..tokDot, tokLBracket}
  PrefixOpTokens* = {tokMinus, tokNot}
  PostfixOpTokens*: set[TokenKind] = {}

proc next*(lexer: var Lexer): (Token, Status)

func `[]`*(lexer: Lexer, span: Span): string {.inline.} =
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
  else: Eof

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
          break # NOTE: Probably should error
    elif curr == '\n':
      inc pos.line
  if pos.offset < lexer.src.len and at(pos.offset) == '"':
    inc pos.offset
  else:
    lexer.status = UnterminatedStr

proc lexChar(lexer: var Lexer) =
  var pos = lexer.token.span.e
  inc pos.offset
  lexer.status =
    case (let curr = at(pos.offset); inc pos.offset; curr)
    of '\n':
      if pos.offset < lexer.src.len and at(pos.offset) == '\'':
        CharNewLine
      else:
        UnterminatedChar
    of '\\':
      # get escaped char
      let curr = at(pos.offset)
      if curr in EscapableChars:
        inc pos.offset
        Ok
      # compare char
      elif curr == 'x' or curr == 'X':
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
      else: InvalidEscSeq
    else: Ok
  if lexer.status.isOk and lexer.at(pos.offset) != '\'':
    lexer.status = UnterminatedChar
  else:
    inc pos.offset
  lexer.token.span.e = pos
  lexer.token.kind = tokChrLit

# NOTE: Consider making this an iterator
proc next*(lexer: var Lexer): (Token, Status) =
  result = (lexer.token, lexer.status)
  lexer.pos = lexer.token.span.e
  if lexer.pos.offset >= lexer.src.len:
    lexer.token = Token(kind: tokEOF, span: (lexer.pos, lexer.pos))
    return
  lexer.token.span.s = lexer.skipWhitespace()
  lexer.token.span.e = lexer.token.span.s
  let ch = lexer.current
  case ch
  of '0'..'9':
    lexer.lexNumber()
  of '-', '+':
    if lexer.peek() in '0'..'9':
      lexer.lexNumber()
    else:
      inc lexer.token.span.e.offset
      # lexer.token.kind = if ch == '-': tokMinus else: tokPlus
      lexer.token.kind = TokenKind(ord(tokMinus) - ord(ch == '+'))
  of '"':
    lexer.lexStr()
  of 'a'..'z', 'A'..'Z', '_':
    lexer.lexIdent()
    lexer.token.kind = keywords.getOrDefault(lexer[lexer.token.span], tokIdent)
  of '\'':
    lexer.lexChar()
  of '<'..'>': # '<', '=', '>'
    lexer.token.kind = TokenKind(ord(tokLt) + (ord(ch) - ord('<')))
    inc lexer.token.span.e.offset
    if lexer.at(lexer.token.span.e.offset) == '=':
      inc lexer.token.span.e.offset
      inc lexer.token.kind, 3
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
                         of '?': tokQuestion
                         of ';': tokSemicolon
                         of '*': tokAsterisk
                         of '/': tokSlash
                         else  : tokEOF

proc revert*(lexer: var Lexer, token: Token, pos: Pos) =
  lexer.token = token
  lexer.pos = pos

iterator items*(lexer: var Lexer): Token =
  while lexer.token.kind != tokEOF and
    (let (t, s) = lexer.next(); s.isOk):
    yield t
