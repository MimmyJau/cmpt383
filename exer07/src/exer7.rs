fn hailstone_sequence_append(n: u64) -> Vec<u64> {
    let mut v = Vec::new();
    let mut n = n;
    v.push(n);

    while n != 1 {
        if n % 2 == 0 {
            n = n / 2;
            v.push(n);
        } else {
            n = 3 * n + 1;
            v.push(n)
        }
    };

    return v;
}

pub fn hailstone(n: u64) -> Vec<u64> {
    return hailstone_sequence_append(n);
}

