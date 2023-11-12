/// An inclusive interval of integers
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct Interval {
    min: i64,
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

    /// Constructs the smallest interval containing all elements of the slice
    /// Requires the slice to be non-empty
    pub fn from_slice(slice: &[i64]) -> Self {
        let mut min = i64::MAX;
        let mut max = i64::MIN;
        for &a in slice {
            min = min.min(a);
            max = max.max(a);
        }
        Self { min, max }
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
        assert!(self.max >= self.min);
        assert!(other.max >= other.min);
        Self {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    /// Returns the number of integers contained in the interval
    pub const fn len(&self) -> i64 {
        self.max
            .saturating_add(1)
            .saturating_sub(self.min)
            .saturating_abs()
    }

    /// Returns true if the interval contains only positive numbers
    pub const fn is_positive(&self) -> bool {
        self.min > 0 && self.max > 0
    }

    /// Returns true if the interval contains only negative numbers
    pub const fn is_negative(&self) -> bool {
        self.max < 0 && self.min < 0
    }

    /// Returns true if the interval contains negative numbers or zero
    pub const fn contains_nonpositive(&self) -> bool {
        assert!(self.max >= self.min);
        self.min <= 0
    }

    /// Returns true if the interval contains positive numbers or zero
    pub const fn contains_nonnegative(&self) -> bool {
        assert!(self.max >= self.min);
        self.max >= 0
    }

    /// Converts the interval to a range
    pub const fn to_range(self) -> std::ops::RangeInclusive<i64> {
        assert!(self.min <= self.max);
        self.min..=self.max
    }
}

impl std::ops::Add<Self> for Interval {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from_slice(&[
            self.min.saturating_add(rhs.min),
            self.min.saturating_add(rhs.max),
            self.max.saturating_add(rhs.max),
            self.max.saturating_add(rhs.min),
        ])
    }
}

impl std::ops::Sub<Self> for Interval {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from_slice(&[
            self.min.saturating_sub(rhs.max),
            self.min.saturating_sub(rhs.min),
            self.max.saturating_sub(rhs.min),
            self.max.saturating_sub(rhs.max),
        ])
    }
}

impl std::ops::Mul<Self> for Interval {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::from_slice(&[
            self.min.saturating_mul(rhs.min),
            self.min.saturating_mul(rhs.max),
            self.max.saturating_mul(rhs.max),
            self.max.saturating_mul(rhs.min),
        ])
    }
}

impl std::ops::Div<Self> for Interval {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::from_slice(&[
            self.min.saturating_div(rhs.max),
            self.min.saturating_div(rhs.min),
            self.max.saturating_div(rhs.min),
            self.max.saturating_div(rhs.max),
        ])
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
        assert_eq!(i * j, Interval::new(-2000, 3000));
        assert_eq!(i / j, Interval::new(-5, 3));
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
