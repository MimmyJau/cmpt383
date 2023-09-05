use std::io;

mod hailstone;


fn main() {
    println!("Please input a number.");

    let mut value = String::new();

    io::stdin()
        .read_line(&mut value)
        .expect("Failed to read line");

    let value: u64 = value.trim().parse().expect("NaN");


    println!(
        "hailstone sequence is: {:?}", 
        hailstone::hailstone_sequence_append(value)
    );
    println!(
        "hailstone sequence is: {:?}", 
        hailstone::hailstone_sequence_prealloc(value)
    );
}
