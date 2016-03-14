use std::collections::HashMap;
use super::item::Item;


pub struct Character {
    pub attributes: Item,
    pub inventory: HashMap<String, (Item, i32)>
}

impl Character {
    pub fn new() -> Character {
        Character {
            attributes: Item::new(),
            inventory: HashMap::new(),
        }
    }

    //Give the character count number of item.
    pub fn give_item(&mut self, item: &Item, count: i32) {
        let name = item.qualitative_descriptors.get(&"name".to_string()).unwrap();
        let mut i = self.inventory.entry(name.to_string()).or_insert((item.clone(), 0));
        i.1 += count;
    }
}
