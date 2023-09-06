use std::io;

mod hailstone;
mod find;
mod rational;

fn test_hailstone() {
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

fn test_find() {
    let v1: Vec<i32> = Vec::from([4, 5, 2, 8, 7, 3, 1]);
    println!("{:?}", find::find_elt(&v1, 8)); // Some(3)
    println!("{:?}", find::find_elt(&v1, 6)); // None
    let v2: Vec<char> = "Hello World!".chars().collect();
    println!("{:?}", find::find_elt(&v2, 'o')); // Some(4)
    println!("{:?}", find::find_elt(&v2, 'q')); // None
}

fn test_rational() {
    let mut r = rational::Rational::new(6,8);
    println!("{:?}", r); // prints Rational { n: 6, d: 8 }
    r.reduce();
    println!("{:?}", r); // prints Rational { n: 3, d: 4 }
    let n = rational::Rational::from(4_i64);
    println!("{:?}", n); // prints Rational { n: 4, d: 1 }
    println!("{}", n == rational::Rational::new(4,1)); // prints true
    let mut x = rational::Rational::new(0,8);
    println!("{:?}", x); // prints Rational { n: 0, d: 8 }
    x.reduce();
    println!("{:?}", x); // prints Rational { n: 0, d: 8 }
    let mut y = rational::Rational::new(5,0);
    println!("{:?}", y); 
    y.reduce();
    println!("{:?}", y);
}


fn main() {
    test_rational()
}
