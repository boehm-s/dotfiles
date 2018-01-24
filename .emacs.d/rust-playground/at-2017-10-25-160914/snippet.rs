// -*- mode:rust;mode:rust-playground -*-
// snippet of code @ 2017-10-25 16:09:14

// === Rust Playground ===
// Execute the snippet with Ctl-Return
// Remove the snippet completely with its dir and all files M-x `rust-playground-rm`

fn main() {
    let test = vec![1,3,4];
    println!("-- {} --", test.into_iter().map(|x| format!("{}, ", x)).collect::<String>());
}
