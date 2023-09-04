from strutils import HexDigits
import std/enumutils
from
  parseutils
import
  parseHex,
  parseOct,
  parseBin,
  parseBiggestInt,
  parseFloat
import questionable/options
import ./common

proc intExpr*(parser: var Parser): BiggestInt =
  func biggestInt(s: openArray[char], n: var BiggestInt, _: int): int =
    s.parseBiggestInt(n)
  template fn(n: untyped): untyped =
    proc(a: openArray[char], b: var BiggestInt, c: int): int =
      `parse n`[BiggestInt](a, b, c)
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset
    finish = tk.span.e.offset
    parseFn =
      if tk.span.e.offset - start >= 3 and parser.at(start) == '0':
        case parser.at(start + 1)
        of 'x', 'X': fn(Hex) # parseHex[BiggestInt]
        of 'o', 'O': fn(Oct) # parseOct[BiggestInt]
        of 'b', 'B': fn(Bin) # parseBin[BiggestInt]
        else: biggestInt
      else: biggestInt
  discard parseFn(parser.lexer.src.toOpenArray(start, finish - 1), result, 0)

proc floatExpr*(parser: var Parser): float =
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset
  discard parseFloat(parser.lexer.src, result, start)

proc escSeq*(parser: var Parser, start: int): (?char, int) =
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
        discard parser
          .lexer
          .src
          .toOpenArray(offset + 1, offset + 2)
          .parseHex(hex)
        inc offset, 2
        some(hex.char)
      else: none(char)
    else: none(char)
  inc offset
  result[1] = offset

proc chrExpr*(parser: var Parser): ?char =
  let
    tk = !parser.lexer.next()
    start = tk.span.s.offset + 1
  if parser.at(start) == '\\':
    if parser.at(start + 1) == '\'': some('\'')
    else:
      escSeq(parser, start + 1)[0]
  else:
    some(parser.at(start))

proc strExpr*(parser: var Parser): string =
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
            # TODO: !IMPORTANT use the error throwing system
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
