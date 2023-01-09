use std::env;

use kaleidoscope;

fn main() {
    if let Some(path) = env::args().nth(1) {
        kaleidoscope::run(&path);
    } else {
        eprintln!("usage: cargo run xxx.kal");
    }
}
