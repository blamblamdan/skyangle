use serde::Serialize;
use std::{
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub enum SkyAngle<T: Conversion<T>> {
    Radian(T),
    Degree(T),
    Arcminute(T),
    Arcsecond(T),
    MilliArcsec(T),
}

impl<T: Conversion<T> + Display> Display for SkyAngle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SkyAngle::Radian(a) => write!(f, "{:.3}radian", a),
            SkyAngle::Degree(a) => write!(f, "{:.3}degree", a),
            SkyAngle::Arcminute(a) => write!(f, "{:.3}arcmin", a),
            SkyAngle::Arcsecond(a) => write!(f, "{:.3}arcsec", a),
            SkyAngle::MilliArcsec(a) => write!(f, "{:.3}mas", a),
        }
    }
}

/* impl<T: Conversion<T> + Display> Display for SkyAngle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SkyAngle::Radian(val) => val.fmt(f),
            SkyAngle::Degree(val) => val.fmt(f),
            SkyAngle::Arcminute(val) => val.fmt(f),
            SkyAngle::Arcsecond(val) => val.fmt(f),
            SkyAngle::MilliArcsec(val) => val.fmt(f),
        }
    }
} */

impl<T: Conversion<T>> SkyAngle<T> {
    pub fn to_radians(self) -> T {
        match self {
            Self::Radian(val) => val, // Can be hardcoded
            Self::Degree(val) => T::from_degree(val),
            Self::Arcminute(val) => T::from_arcmin(val),
            Self::Arcsecond(val) => T::from_arcsec(val),
            Self::MilliArcsec(val) => T::from_mas(val),
        }
    }
    pub fn into_degree(self) -> Self {
        SkyAngle::Degree(self.to_radians().to_degree())
    }
    pub fn into_arcmin(self) -> Self {
        SkyAngle::Arcminute(self.to_radians().to_arcmin())
    }
    pub fn into_arcsec(self) -> Self {
        SkyAngle::Arcsecond(self.to_radians().to_arcsec())
    }
    pub fn into_mas(self) -> Self {
        SkyAngle::MilliArcsec(self.to_radians().to_mas())
    }

    // pub fn into_<abbrv>(self) -> Self {
    //     SkyAngle::<unit>(self.to_radians().to_<abbrv>())
    // }
    pub fn into_value(self) -> T {
        // match self {
        //     SkyAngle::Radian(val) => val,
        //     SkyAngle::Degree(val) => val,
        //     SkyAngle::Arcminute(val) => val,
        //     SkyAngle::Arcsecond(val) => val,
        //     SkyAngle::MilliArcsec(val) => val,
        // }
        match self {
            SkyAngle::Radian(val)       | // Hardcoded
            SkyAngle::Degree(val)       |
            SkyAngle::Arcminute(val)    |
            SkyAngle::Arcsecond(val)    |
            SkyAngle::MilliArcsec(val)  => val
        }
    }
}

macro_rules! impl_self_ops {
    ($name:ty ; $(($t_op:tt, $abbrv:tt, $op:tt)),+) => {
        $(impl<T> $t_op for $name
        where
            T: Conversion<T> + $t_op<Output = T>,
        {
            type Output = T;

            fn $abbrv(self, rhs: Self) -> Self::Output {
                self.to_radians() $op rhs.to_radians()
            }
        })+
    };
}

impl_self_ops!(SkyAngle<T> ; (Div, div, /), (Sub, sub, -), (Add, add, +));


macro_rules! impl_extern_ops {
    ($name:ty ; $(($t_op:tt, $abbrv:tt, $op:tt)),+) => {
        $(impl<T> $t_op<T> for $name
            where
                T: Conversion<T> + $t_op<Output = T>,
            {
                type Output = T;
            
                fn $abbrv(self, rhs: T) -> Self::Output {
                    self.to_radians() $op rhs
                }
            })+
    };
}

impl_extern_ops!(SkyAngle<T> ; (Div, div, /), (Mul, mul, *));

/// Conversion between angle units
pub trait Conversion<T> {
    fn from_degree(self) -> T;
    fn from_arcmin(self) -> T;
    fn from_arcsec(self) -> T;
    fn from_mas(self) -> T;
    fn to_degree(self) -> T;
    fn to_arcmin(self) -> T;
    fn to_arcsec(self) -> T;
    fn to_mas(self) -> T;
}

macro_rules! impl_conversion {
    ($($name:ty),+) => {
        $(impl Conversion<$name> for $name {
            /// Converts angle in arcminute to radian
            fn from_degree(self) -> $name {
                self.to_radians()
            }            /// Converts angle in arcminute to radian
            fn from_arcmin(self) -> $name {
                self.to_radians() / 60.
            }
            /// Converts angle in arcsecond to radian
            fn from_arcsec(self) -> $name {
                self.from_arcmin() / 60.
            }
            /// Converts angle in milli-arcsecond to radian
            fn from_mas(self) -> $name {
                self.from_arcsec() * 1e-3
            }
            fn to_degree(self) -> $name {
                self.to_degrees()
            }            /// Converts angle in radian to arcminute
            fn to_arcmin(self) -> $name {
                60.0 * self.to_degrees()
            }
            /// Converts angle in radian to arcsecond
            fn to_arcsec(self) -> $name {
                60.0 * self.to_arcmin()
            }
            /// Converts angle in radian to mill-arcsecond
            fn to_mas(self) -> $name {
                1e3 * self.to_arcsec()
            }
        })+
        $(impl Conversion<Vec<$name>> for Vec<$name> {
            fn from_degree(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_degree()).collect()
            }
            fn from_arcmin(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_arcmin()).collect()
            }

            fn from_arcsec(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_arcsec()).collect()
            }

            fn from_mas(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_mas()).collect()
            }

            fn to_degree(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_degree()).collect()
            }
            fn to_arcmin(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_arcmin()).collect()
            }

            fn to_arcsec(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_arcsec()).collect()
            }

            fn to_mas(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_mas()).collect()
            }
        })+
        $(impl Conversion<Vec<$name>> for &[$name] {
            fn from_degree(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_degree()).collect()
            }
            fn from_arcmin(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_arcmin()).collect()
            }

            fn from_arcsec(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_arcsec()).collect()
            }

            fn from_mas(self) -> Vec<$name> {
                self.into_iter().map(|x| x.from_mas()).collect()
            }

            fn to_degree(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_degree()).collect()
            }

            fn to_arcmin(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_arcmin()).collect()
            }

            fn to_arcsec(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_arcsec()).collect()
            }

            fn to_mas(self) -> Vec<$name> {
                self.into_iter().map(|x| x.to_mas()).collect()
            }
        })+    };
}
impl_conversion!(f64, f32);


#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn ser() {
        let a = SkyAngle::Arcminute(1f64);
        let s = serde_json::to_string(&a).unwrap();
        println!("{s}");
    }

    #[test]
    fn self_operations() {
        let a = SkyAngle::Degree(1f64);
        let b = SkyAngle::Degree(1f64);

        // Add
        let _ = a + b;
        // Subtract
        let _ = a - b;
        // Divide
        let _ = a / b;
    }

    #[test]
    fn ext_operations() {
        let a = SkyAngle::Degree(1f64);

        // Divide
        let _ = a / 2f64;
        // Multiply
        let _ = a * 2f64;
    }

    #[test]
    fn conversion() {
        let a = SkyAngle::Arcminute(1f64);

        // Check value is preserved through conversions
        let b = a.clone()
            .into_arcmin()
            .into_arcsec()
            .into_degree()
            .into_mas()
            .into_arcmin();

        assert_eq!(b, a);
    }
}
