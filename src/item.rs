use std::collections::HashMap;

#[derive(Clone)]
pub struct Item {
    pub qualitative_descriptors: HashMap<String, String>,
    pub quantitative_descriptors: HashMap<String, f32>,
}

impl Item {
    pub fn new() -> Item {
        Item {
            qualitative_descriptors: HashMap::new(),
            quantitative_descriptors: HashMap::new(),
        }
    }

    //Return a string describing the item
    pub fn detail(&self) -> String {
        let mut working: String = "".to_string();

        &working.push_str("Qualities:\n");
        for (name, data) in &self.qualitative_descriptors {
            working = working + &format!("\t{}: {}\n", name, data);
        }
        &working.push_str("Quantities:\n");
        for (name, data) in &self.quantitative_descriptors {
            working = working + &format!("\t{}: {}\n", name, data);
        }
        working
    }
}
