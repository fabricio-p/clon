import sequtils
import ./clon/[lexer, status]

when isMainModule:
  var lex: Lexer
  lex = initLexer("0xff 0b1001 12.3 1e3")
  doAssert lex.items.toSeq == [
    Token(kind: tokIntLit,   span: ((0, 1),  (4, 1))),
    Token(kind: tokIntLit,   span: ((5, 1),  (11, 1))),
    Token(kind: tokFloatLit, span: ((12, 1), (16, 1))),
    Token(kind: tokIntLit,   span: ((17, 1), (20, 1))),
  ]
  lex = initLexer("fc loc if case for whl box ab__cd23jnWq")
  doAssert lex.items.toSeq == [
    Token(kind: tokFc,    span: ((0, 1),  (2, 1))),
    Token(kind: tokLoc,   span: ((3, 1),  (6, 1))),
    Token(kind: tokIf,    span: ((7, 1),  (9, 1))),
    Token(kind: tokCase,  span: ((10, 1), (14, 1))),
    Token(kind: tokFor,   span: ((15, 1), (18, 1))),
    Token(kind: tokWhl,   span: ((19, 1), (22, 1))),
    Token(kind: tokBox,   span: ((23, 1), (26, 1))),
    Token(kind: tokIdent, span: ((27, 1), (39, 1))),
  ]
  lex = initLexer("() [] {} +-*/ ")
  doAssert lex.items.toSeq == [
    Token(kind: tokLParen,   span: ((0, 1),  (1, 1))),
    Token(kind: tokRParen,   span: ((1, 1),  (2, 1))),
    Token(kind: tokLBracket, span: ((3, 1),  (4, 1))),
    Token(kind: tokRBracket, span: ((4, 1),  (5, 1))),
    Token(kind: tokLBrace,   span: ((6, 1),  (7, 1))),
    Token(kind: tokRBrace,   span: ((7, 1),  (8, 1))),
    Token(kind: tokPlus,     span: ((9, 1),  (10, 1))),
    Token(kind: tokMinus,    span: ((10, 1), (11, 1))),
    Token(kind: tokAsterisk, span: ((11, 1), (12, 1))),
    Token(kind: tokSlash,    span: ((12, 1), (13, 1))),
  ]
  lex = initLexer(r""" "hullo" """)
  doAssert lex.next() == (Token(kind: tokStrLit, span: ((1, 1), (8, 1))), Ok)
  lex = initLexer(r"'k' '\n'")
  doAssert lex.items.toSeq == [
    Token(kind: tokChrLit, span: ((0, 1), (3, 1))),
    Token(kind: tokChrLit, span: ((4, 1), (8, 1))),
  ]
