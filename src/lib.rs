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
            Self::Radian(val) => val,
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
    pub fn into_value(self) -> T {
        match self {
            SkyAngle::Radian(val)       |
            SkyAngle::Degree(val)       |
            SkyAngle::Arcminute(val)    |
            SkyAngle::Arcsecond(val)    |
            SkyAngle::MilliArcsec(val)  => val
        }
    }
}

macro_rules! impl_f_ops {
    ($name:ty ; $f:expr ; $(($t_impl_op:ty, $t_out_op:ident, $t_in: ty, $abbrv:ident, $op:tt)),+) => {
        $(
            impl<T> $t_impl_op for $name
            where
                T: Conversion<T> + $t_out_op<Output = T>,
            {
                type Output = T;
                fn $abbrv(self, rhs: $t_in) -> Self::Output {
                    self.to_radians() $op $f(rhs)
                }
            }
        )+
    };
}

macro_rules! impl_self_ops {
    ($name:ty; $(($t_op:tt, $abbrv:tt, $op:tt)),+) => {
        impl_f_ops!($name;Self::to_radians;
            $(
                ($t_op, $t_op, $name, $abbrv, $op)
            ),+
        );
    }
}

macro_rules! impl_extern_ops {
    ($name:ty; $(($t_op:tt, $abbrv:tt, $op:tt)),+) => {
        impl_f_ops!($name;T::from;
            $(
                ($t_op<T>, $t_op, T, $abbrv, $op)
            ),+
        );
    }
}

impl_self_ops!(SkyAngle<T>; (Div, div, /), (Sub, sub, -), (Add, add, +)); // (SkyAngle<T>, SkyAngle<T>)
impl_extern_ops!(SkyAngle<T>; (Div, div, /), (Mul, mul, *)); // (SkyAngle<T>, T)

macro_rules! make_trait_Conversion {
    ($(($f:ident,$t:ident)),+) => {
        pub trait Conversion<T> {
            $(
                fn $f(self) -> T;
                fn $t(self) -> T;
            )+
        }
    };
}

/// Implement wrapping
macro_rules! with_dollar_sign {
    ($($body:tt)*) => {
        macro_rules! __with_dollar_sign { $($body)* }
        __with_dollar_sign!($);
    }
}

macro_rules! make_to_from_T_V {
    ($($t_name:ty | $v_name: ty);+) => {
        with_dollar_sign!{
            ($d:tt) => {
                macro_rules! to_from_T_V {
                    ($d(($f:ident,$t:ident)),+) => {
                        $(
                            impl Conversion<$t_name> for $v_name {
                                $d(
                                    fn $f(self) -> $t_name {
                                        self.into_iter().map(|x| x.$f()).collect()
                                    }
                                    fn $t(self) -> $t_name {
                                        self.into_iter().map(|x| x.$t()).collect()
                                    }
                                )+
                            }
                        )+
                    }
                }
            }
        }
    }
}


macro_rules! make_impl_conversion {
    ($(($f:ident,$t:ident)),+) => {
        with_dollar_sign! {
            ($d: tt) => {
                macro_rules! impl_conversion {
                    ($d($name:ty),+) => {
                        $d(impl Conversion<$name> for $name {
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
                
                        make_to_from_T_V!(
                            // Add more input/output types to here
                            $d(
                                Vec<$name>|Vec<$name>; 
                                Vec<$name>|&[$name]
                            );+
                        );
                        to_from_T_V!(
                            // 
                            (from_degree,to_degree),
                            (from_arcmin,to_arcmin),
                            (from_arcsec,to_arcsec),
                            (from_mas,to_mas)
                        );
                        
                    }
                }
            }
        }
    };
}

macro_rules! setup_conversion {
    ($(($f:ident,$t:ident)),+) => {
        make_trait_Conversion!($(($f,$t)),+);
        make_impl_conversion!($(($f,$t)),+);
    };
}

setup_conversion!(
    (from_degree,to_degree),
    (from_arcmin,to_arcmin),
    (from_arcsec,to_arcsec),
    (from_mas,to_mas)
);
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

        let eps = 1e-5;
        assert_eq!((b-a).abs() < eps, true);
    }
}
