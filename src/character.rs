use std::collections::{HashMap, HashSet};
use super::item::Item;
use std::cell::Cell;

pub struct Character <'a> {
    pub attributes: Item,
    pub inventory: HashMap<String, (Item, i32)>,
    pub feats: HashMap<String, HashMap<String, (bool, i32, Cell<&'a i32>)>>,
    pub scores: HashMap<String, i32>,
    pub ability_score_names: HashSet<String>,
    pub equipment: HashMap<String, Item>,
}

impl <'a> Character <'a> {
    pub fn new () -> Character<'a> {
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
    pub fn modify_inventory(&mut self, item: &Item, mut count: i32) {
        let name = item.qualitative_descriptors.get(&"name".to_string()).unwrap();
        {
            let mut i = self.inventory.entry(name.to_string()).or_insert((item.clone(), 0));
            i.1 += count;
            count = i.1;
        }
        if count <= 0 {
            self.inventory.remove(&name.to_string());
        }
    }

    pub fn equip(&mut self, item: &Item) {
        match item.qualitative_descriptors.get("slot") {
            Some(slot) => {
                match self.equipment.insert(slot.to_string(), item.clone()) {
                    Some(i) => self.modify_inventory(&i, 1),
                    None => {},
                }
            },
            None => {},
        };
    }
}
