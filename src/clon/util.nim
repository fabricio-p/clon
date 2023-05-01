import terminal

template forever*(body: untyped): untyped =
  while true:
    body

template withas*(expr: typed; name: untyped): untyped =
  (let name = expr; name)

template doWhile*(cond, body: untyped): untyped =
  while true:
    body
    if not (cond): break

template todo*(msg: static[string] = ""): untyped =
  let (file, line, _) = instantiationInfo()
  stderr.styledWriteLine(
    fgCyan, "Code path not yet implemented (", msg, ") ",
    fgWhite, "[",
    fgGreen, file,
    fgWhite, ":",
    fgGreen, $line,
    fgWhite, "]")
  quit(QuitFailure)
