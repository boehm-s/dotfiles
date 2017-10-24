// -*- mode:rust;mode:rust-playground -*-
// snippet of code @ 2017-10-23 20:13:02

// === Rust Playground ===
// Execute the snippet with Ctl-Return
// Remove the snippet completely with its dir and all files M-x `rust-playground-rm`

fn main() {
    let vec = vec![1,25,320];
    let formatted_str:String = vec
        .into_iter()
        .enumerate()
        .map(|(i, x)| {
            match i {
                0 => format!("[{:03},", x),
                2 => format!("{:03}]", x),
                _ => format!("{:03},", x)
            }
        })
        .collect();

    formatted_str
}
