import tables, strutils
type
  Item* = ref ItemObj
  ItemObj = object
    qualitativeDescriptors*: Table[string, string]
    quantitativeDescriptors*: Table[string, float]

proc newItem*(): Item =
  new(result)
  result.qualitativeDescriptors = initTable[string, string]()
  result.quantitativeDescriptors = initTable[string, float]()

proc detail*(i: Item): string =
  result = ""
  result &= "Qualities:\n"
  for name, data in i.qualitativeDescriptors:
    result &= "\t$1: $2\n" % [name, data]

  result &= "Quantities:\n"
  for name, data in i.quantitativeDescriptors:
    result &= "\t$1: $2\n" % [name, $data]
