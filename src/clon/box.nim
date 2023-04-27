type Box*[T] = object
  data: ref T

proc box*[T](value: sink T): Box[T] =
  new(result.data)
  result.data[] = value

proc getRef*[T](box: Box[T]): ref T =
  result = box.data

proc unbox*[T](box: Box[T]): T =
  runnableExamples:
    let num = box(3)
    doAssert num == box.unbox()
  if isNil(box.data):
    raise newException(ValueError, "Box has no value to unbox")
  else:
    result = box.data[]
