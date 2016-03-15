use std::collections::{HashMap, HashSet};
use super::item::Item;


pub struct Character {
    pub attributes: Item,
    pub inventory: HashMap<String, (Item, i32)>,
    pub feats: HashMap<String, HashMap<String, i32>>,
    pub scores: HashMap<String, i32>,
    pub ability_score_names: HashSet<String>,
    pub equipment: HashMap<String, Item>,
}

impl Character {
    pub fn new() -> Character {
        Character {
            attributes: Item::new(),
            inventory: HashMap::new(),
            feats: HashMap::new(),
            scores: HashMap::new(),
            ability_score_names: HashSet::new(),
            equipment: HashMap::new(),
        }
    }

    //Give the character count number of item.
    pub fn give_item(&mut self, item: &Item, count: i32) {
        let name = item.qualitative_descriptors.get(&"name".to_string()).unwrap();
        let mut i = self.inventory.entry(name.to_string()).or_insert((item.clone(), 0));
        i.1 += count;
    }

    pub fn equip(&mut self, item: &Item) {
        match item.qualitative_descriptors.get("slot") {
            Some(slot) => {
                match self.equipment.insert(slot.to_string(), item.clone()) {
                    Some(i) => self.give_item(&i, 1),
                    None => {},
                }
            },
            None => {},
        };
    }
}
