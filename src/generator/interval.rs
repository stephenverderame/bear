/// An inclusive interval of integers
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct Interval {
    /// None for -infinity
    min: i64,
    /// None for +infinity
    max: i64,
}

impl std::fmt::Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ..= {}", self.min, self.max)
    }
}

impl Interval {
    /// Returns an interval from a known constant
    pub const fn from_const(a: i64) -> Self {
        Self { min: a, max: a }
    }

    /// Returns an interval with unknown bounds
    pub const fn make_unknown() -> Self {
        Self {
            min: i64::MIN,
            max: i64::MAX,
        }
    }

    /// Returns true if the interval contains the given integer
    pub const fn contains(&self, a: i64) -> bool {
        self.min <= a && a <= self.max
    }

    /// Constructs an interval from two integers.
    /// The order of the arguments does not matter
    pub fn new(a: i64, b: i64) -> Self {
        Self {
            min: a.min(b),
            max: a.max(b),
        }
    }

    /// Constructs an interval from two integers, if the lower bound is greater
    /// than the upper bound, the upper bound is clamped to the lower bound
    pub fn new_clamped_lower(lower: i64, upper: i64) -> Self {
        Self {
            min: lower,
            max: upper.max(lower),
        }
    }

    pub const fn lower_bound(&self) -> i64 {
        self.min
    }

    pub const fn upper_bound(&self) -> i64 {
        self.max
    }

    /// Returns the union of two intervals (the smallest interval that contains
    /// both intervals)
    pub fn union(&self, other: Self) -> Self {
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    pub const fn len(&self) -> i64 {
        self.max
            .saturating_add(1)
            .saturating_sub(self.min)
            .saturating_abs()
    }
}

impl std::ops::Add<Self> for Interval {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            min: self.min.saturating_add(rhs.min),
            max: self.max.saturating_add(rhs.max),
        }
    }
}

impl std::ops::Sub<Self> for Interval {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::new(
            self.min.saturating_sub(rhs.max),
            self.max.saturating_sub(rhs.min),
        )
    }
}

impl std::ops::Mul<Self> for Interval {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(
            self.min.saturating_mul(rhs.min),
            self.max.saturating_mul(rhs.max),
        )
    }
}

impl std::ops::Div<Self> for Interval {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(
            self.min.saturating_div(rhs.max),
            self.max.saturating_div(rhs.min),
        )
    }
}

#[cfg(test)]
mod test {
    use super::Interval;
    #[test]
    #[allow(clippy::many_single_char_names)]
    fn test_interval() {
        let a = 0;
        let b = 100;
        let c = -20;
        let d = 30;
        let i = Interval::new(a, b);
        let j = Interval::new(c, d);
        assert_eq!(i + j, Interval::new(a + c, b + d));
        assert_eq!(i - j, Interval::new(a - d, b - c));
        assert_eq!(i * j, Interval::new(a * c, b * d));
        assert_eq!(i / j, Interval::new(a / d, b / c));
    }

    #[test]
    fn test_div() {
        let a_intval = Interval::new(0, 100);
        let b_intval = Interval::new(0, 100);
        let c_intval = Interval::from_const(-3);
        assert_eq!((a_intval + b_intval) / c_intval, Interval::new(-66, 0));
        // (+ (* (/ 99 (- 42 -8)) (/ (/ c -27) -83)) 4115739418498339)
        let a_intval = Interval::new(0, 100);
        let c = a_intval / Interval::from_const(-27); // (-3, 0)
        let c = c / Interval::from_const(-83); // (0, 0)
        let b = Interval::from_const(99) // (0, 0)
            / (Interval::from_const(42) - Interval::from_const(-8));
        let b = b * c;
        assert_eq!(b, Interval::new(0, 0));
    }
}
