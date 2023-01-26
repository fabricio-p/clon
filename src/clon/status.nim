type
  Status* {.pure.} = enum
    Ok
    InvalidChar
    InvalidNumLit
    InvalidEscSeq
    UnterminatedStr
    UnterminatedChar
    CharNewline

func isOk*(status: Status): bool {.inline.} = status == Status.Ok
