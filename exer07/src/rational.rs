use std::cmp;

fn gcd(a: i64, b: i64) -> i64 {
    // If either number is 0, gcd is 0.
    if (a == 0) || (b == 0) {
        return 0;
    }

    // Otherwise, use Euclidean Algorithm to calculate gcd.
    let mut dividend = cmp::max(a, b);
    let mut divisor = cmp::min(a, b);
    let mut r = dividend % divisor;

    while r != 0 {
        dividend = divisor;
        divisor = r;
        r = dividend % divisor;
    }

    return divisor;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rational {
    n: i64,
    d: i64,
}

impl Rational {
    pub fn new(n: i64, d: i64) -> Rational {
        Rational {
            n,
            d,
        }
    }

    pub fn reduce(&mut self) {
        let gcd = gcd(self.n, self.d);
        if gcd > 1 {
            self.n = self.n / gcd;
            self.d = self.d / gcd;
        }
    }
}

impl From<i64> for Rational {
    fn from(n: i64) -> Rational {
        Rational {
            n: n,
            d: 1,
        }
    }
}
