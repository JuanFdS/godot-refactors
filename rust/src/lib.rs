// use godot::prelude::*;
// use godot::classes::Sprite2D;
// use godot::classes::ISprite2D;
// use pest::Parser;
use pest_derive::Parser;
mod coso;

#[derive(Parser)]
#[grammar = "csv.pest"]
pub struct CSVParser;

struct MyExtension;

// #[gdextension]
// unsafe impl ExtensionLibrary for MyExtension {}

// #[derive(GodotClass)]
// #[class(tool, base=Sprite2D)]
// struct Player {
//     speed: f64,
//     angular_speed: f64,

//     base: Base<Sprite2D>
// }

// #[godot_api]
// impl Player {   
//     #[func]
//     fn parsear(&self, s: String) -> GString {
//         let successful_parse = CSVParser::parse(Rule::field, &s);

//         let coso = format!("{:?}", successful_parse);

//         GString::from(coso)
//     }
// }

// #[godot_api]
// impl ISprite2D for Player {
//     fn init(base: Base<Sprite2D>) -> Self {
//         let successful_parse = CSVParser::parse(Rule::field, "-273.15");

//         godot_print!("{:?}", successful_parse); // Prints to the Godot console
        
//         Self {
//             speed: 400.0,
//             angular_speed: std::f64::consts::PI,
//             base,
//         }
//     }
// }