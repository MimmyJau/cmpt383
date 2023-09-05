use std::io;

mod exer7;


fn main() {
    println!("Please input a number.");

    let mut value = String::new();

    io::stdin()
        .read_line(&mut value)
        .expect("Failed to read line");

    let value: u64 = value.trim().parse().expect("NaN");


    println!("hailstone sequence is: {:?}", exer7::hailstone(value));
}
