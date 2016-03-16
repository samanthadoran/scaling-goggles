type
  Feat* = ref FeatObj
  FeatObj = object
    classSkill*: bool
    score*: int
    bonus*: ref int
    modifierName*: string

proc newFeat*(classSkill: bool, score: int, bonus: ref int, modifierName: string): Feat =
  new(result)
  result.classSkill = classSkill
  result.score = score
  result.bonus = bonus
  result.modifierName = modifierName

proc calculate*(f: Feat): int =
  result =
    if f.classSkill and f.score > 0:
      f.score + f.bonus[] + 3
    else:
      f.score + f.bonus[]
