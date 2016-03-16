mod character;
mod item;
use item::Item;
use character::Character;
use std::cell::Cell;
use std::collections::HashMap;

fn main() {
    //Some random values for a character
    let mut f = Character::new();
    f.attributes.quantitative_descriptors.insert("age".to_string(), 22.);
    f.attributes.qualitative_descriptors.insert("name".to_string(), "Samantha".to_string());

    //Some random values for an item
    let mut i = Item::new();
    i.qualitative_descriptors.insert("name".to_string(), "Potion".to_string());
    i.quantitative_descriptors.insert("Potency".to_string(), 50f32);

    let mut equip = Item::new();
    equip.qualitative_descriptors.insert("name".to_string(), "helmet".to_string());
    equip.qualitative_descriptors.insert("slot".to_string(), "head".to_string());
    equip.quantitative_descriptors.insert("AC".to_string(), 2f32);

    //Give 10 of the item to the character
    f.modify_inventory(&i, 10);
    f.equip(&equip);
    f.scores.insert("strength".to_owned(), 10i32);
    f.feats.insert("strength".to_owned(), HashMap::new());
    //f.give_feat(&"climb".to_owned(), &true, &5, "strength".to_owned());

    println!("Welcome to charactersheet");

    //List qualities and quantities of character
    println!("{}", Item::detail(&f.attributes));

    //List inventory of character
    println!("Inventory:");
    for (name, data) in &f.inventory {
        println!("\t{}: #{}", name, data.1);
        //This is really hacky and I should feel bad...
        let formatted = Item::detail(&data.0).replace("\n", "\n\t").replace("\t", "\t\t");
        println!("\n\t\t{}", formatted);
    }

    println!("Equipment:");
    for (slot, data) in &f.equipment {
        println!("\t{}:", slot);
        //This is really hacky and I should feel bad...
        let formatted = Item::detail(&data).replace("\n", "\n\t").replace("\t", "\t\t");
        println!("\n\t\t{}", formatted);
    }

    //Because getting the lifetimes to work in a function was too hard @_@
    let c = Cell::new(f.scores.get(&"strength".to_owned()).unwrap());
    let feat = (true, 5, c);
    f.feats.get_mut(&"strength".to_owned()).unwrap().insert("climb".to_owned(), feat);

    println!("Feats:");
    for (modifier, data) in &f.feats {
        println!("\t{}:", modifier);
        for (name, feat_info) in data {
            let class_skill = feat_info.0;
            let bonus = feat_info.1;
            let modifier = &feat_info.2;
            let total_bonus =
                if class_skill && bonus > 0 {
                    3 + bonus + modifier.get()
                } else {
                    bonus + modifier.get()
                };
            println!("\t\t{}: {}", name, total_bonus);
        }
    }
}
