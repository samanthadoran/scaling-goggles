import character, feat, item
import tables, strutils

proc main() =
  var c = newCharacter()
  c.attributes.quantitativeDescriptors["age"] = 22
  c.attributes.qualitativeDescriptors["name"] = "Samantha"

  var i = newItem()
  i.qualitativeDescriptors["name"] = "Potion"
  i.quantitativeDescriptors["potency"] = 50

  var equip = newItem()
  equip.qualitativeDescriptors["name"] = "helmet"
  equip.qualitativeDescriptors["slot"] = "head"
  equip.quantitativeDescriptors["AC"] = 2

  c.modifyInventory(i, 10)
  c.equip(equip)

  c.scores["strength"] = new(int)
  c.scores["strength"][] = 10

  var modifier = c.scores["strength"]
  let feat = newFeat(true, 5, modifier, "strength")

  c.giveFeat("climb", feat)

  echo("Scaling-Goggles")
  echo(c.attributes.detail())

  echo("Inventory:")
  for name, data in c.inventory:
    echo("\t$1: $2" % [name, $data.count])
    let detail = data.entry.detail()
    let formatted = detail.replace("\n", "\n\t").replace("\t","\t\t")
    echo("\n\t\t", formatted)

  echo("Equipment:")
  for slot, data in c.equipment:
    echo("\t", slot)
    let formatted = data.detail().replace("\n", "\n\t").replace("\t","\t\t")
    echo("\n\t\t", formatted)

  echo("Feats:")
  for modifier, data in c.feats:
    echo("\t", modifier)
    for name, featInfo in data:
      echo("\t\t$1: $2" % [name, $featInfo.calculate()])

when isMainModule:
  main()
