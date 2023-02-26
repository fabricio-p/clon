import tables
import questionable/options

type
  TypeKind* {.pure.} = enum
    VOID
    INT
    BYTE
    CHAR
    FLOAT
    PTR
    SLICE
    TUPLE
    BOX
    FC
    ALIAS
    REF
  BaseType* = range[VOID..FLOAT]
  TypeID* = uint32
  Type* = object
    tid*: TypeID
    case kind*: TypeKind
    of PTR, REF:
      ptrId*: TypeID
    of TUPLE:
      tupl*: seq[TypeID]
    of BOX:
      box*: seq[tuple[name: string, typ: TypeID]]
    of FC:
      params*: seq[TypeID]
      ret*: TypeID
    of ALIAS:
      name*: string
      aliasId*: TypeID
    else: discard
  Scope* = object
    vars*: seq[(string, TypeID)]
    prev*: ?ptr Scope
  Env* = ref object
    types*: seq[Type]
    aliases*: Table[string, TypeID]
    unres*: seq[TypeID]
    unresAliases*: Table[string, TypeID]
    # globScope*: Scope

proc initScope*(prev: ?ptr Scope): Scope =
  result.prev = prev
  result.vars = newSeq[(string, TypeID)]()

func newEnv*(): Env =
  new(result)
  result.types = @[]
  result.aliases = initTable[string, TypeID]()
  result.unres = @[]
  result.unresAliases = initTable[string, TypeID]()
  # result.globScope = Scope(vars: @[])
