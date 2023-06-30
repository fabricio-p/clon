type Box*[T] = object
  data: ref T

proc box*[T](value: sink T): Box[T] =
  new(result.data)
  result.data[] = value

func getRefUnsafe*[T](box: Box[T]): ref T = box.data

func isEmpty*[T](box: Box[T]): bool = isNil(box.data)

proc unbox*[T](box: sink Box[T]): T =
  runnableExamples:
    let num = box(3)
    doAssert num == box.unbox()
  if isNil(box.data):
    raise newException(ValueError, "Box has no value to unbox")
  else:
    result = box.data[]

func `[]`*[T](box: sink Box[T]): T = box.unbox()

template `.`*[T](box: Box[T], name: untyped): untyped =
  box.getRefUnsafe.name
