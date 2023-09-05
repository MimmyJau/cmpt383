pub fn hailstone_sequence_append(n: u64) -> Vec<u64> {
    let mut v = Vec::new();
    let mut n = n;
    v.push(n);

    while n != 1 {
        n = hailstone(n);
        v.push(n)
    };

    return v;
}

pub fn hailstone_sequence_prealloc(n: u64) -> Vec<u64> {
    // Figure out length of hailstone sequence.
    let mut x = n;
    let mut size: usize = 1;
    while x != 1 {
        x = hailstone(x);
        size += 1 ;
    }

    // Populate pre-allocated, empty vector with sequence.
    let mut v = Vec::with_capacity(size);
    let mut n = n;
    v.push(n);
    while n != 1 {
        n = hailstone(n);
        v.push(n)
    }

    return v;
}

fn hailstone (n: u64) -> u64 {
    if n % 2 == 0 {
        return n / 2;
    } else {
        return 3 * n + 1;
    }
}


