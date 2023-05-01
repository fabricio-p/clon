template forever*(body: untyped): untyped =
  while true:
    body

template withas*(expr: typed; name: untyped): untyped =
  (let name = expr; name)

template doWhile*(cond, body: untyped): untyped =
  while true:
    body
    if not (cond): break
