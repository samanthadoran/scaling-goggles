import item, feat
import tables
type
  Character* = ref CharacterObj
  CharacterObj = object
    attributes*: Item
    inventory*: Table[string, tuple[entry: Item, count: int]]
    feats*: Table[string, Table[string, Feat]]
    scores*: Table[string, ref int]
    equipment*: Table[string, Item]

proc newCharacter*(): Character =
  new(result)
  result.attributes = newItem()
  result.inventory = initTable[string, tuple[entry: Item, count: int]]()
  result.feats = initTable[string, Table[string, Feat]]()
  result.scores = initTable[string, ref int]()
  result.equipment = initTable[string, Item]()

proc giveFeat*(c: Character, name: string, f: Feat) =
  #Give a preconstructed feat to a character
  if not c.feats.hasKey(f.modifierName):
    c.feats[f.modifierName] = initTable[string, Feat]()
  c.feats[f.modifierName][name] = f

proc modifyInventory*(c: Character, i: Item, count: int) =
  #Modifiy the character's inventory
  let name = i.qualitativeDescriptors["name"]
  var entry = mgetOrPut[string, tuple[entry: Item, count: int]](c.inventory, name, (i, count))
  entry.count += count
  if entry.count <= 0:
    c.inventory.del(name)

proc equip*(c: Character, i: Item) =
  #Equip an item, return previously worn item to inventory
  let slot = i.qualitativeDescriptors["slot"]
  if c.equipment.hasKey(slot):
    c.modifyInventory(c.equipment[slot], 1)
  c.equipment[slot] = i
