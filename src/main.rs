mod character;
mod item;
use item::Item;
use character::Character;

fn main() {
    //Some random values for a character
    let mut f = Character::new();
    f.attributes.quantitative_descriptors.insert("age".to_string(), 22.);
    f.attributes.qualitative_descriptors.insert("name".to_string(), "Samantha".to_string());

    //Some random values for an item
    let mut i = Item::new();
    i.qualitative_descriptors.insert("name".to_string(), "Potion".to_string());
    i.quantitative_descriptors.insert("Potency".to_string(), 50f32);

    //Give 10 of the item to the character
    f.give_item(&i, 10);

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
}
