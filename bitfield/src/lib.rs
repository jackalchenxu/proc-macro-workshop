// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
#![allow(non_snake_case)]
// pub use bitfield_impl::bitfield;
pub use bitfield_impl::*;

bitfield_impl::define_bit_field_specifiers!();

pub trait Specifier {
    const BITS: usize;
    type SetterType;
    type GetterType;

    fn from_u64(val: u64) -> Self::GetterType;
    fn into_u64(val: Self::SetterType) -> u64;
}

impl Specifier for bool {
    const BITS: usize = 1;
    type GetterType = bool;
    type SetterType = bool;

    fn from_u64(val: u64) -> Self::GetterType {
        val != 0
    }

    fn into_u64(val: Self::SetterType) -> u64 {
        val as u64
    }
}

pub struct CheckTotalSizeIsMultipleOfEightBits<T: checks::TotalSizeIsMultipleOfEightBits> {
    phantom: std::marker::PhantomData<T>,
}
pub struct CheckDiscriminantInRange<T: checks::DiscriminantInRange> {
    phantom: std::marker::PhantomData<T>,
}
pub struct CheckOccupiedAsItsDeclare<T: checks::OccupiedAsItsDeclare> {
    phantom: std::marker::PhantomData<T>,
}

mod checks {
    pub trait TotalSizeIsMultipleOfEightBits {}
    impl TotalSizeIsMultipleOfEightBits for [u8; 0] {}
    pub trait DiscriminantInRange {}
    impl DiscriminantInRange for [u8; 0] {}

    pub trait OccupiedAsItsDeclare {}
    impl OccupiedAsItsDeclare for [u8; 1] {}
}
