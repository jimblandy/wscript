//! Types that have `to_le_bytes` an `from_le_bytes` methods.

pub trait LeBytes: Sized {
    type Bytes: AsRef<[u8]>;
    const WGSL_NAME: &'static str;

    fn to_le_bytes(self) -> Self::Bytes;
    fn from_le_bytes(bytes: Self::Bytes) -> Self;
}

macro_rules! impl_to_le_bytes {
    ( $( $t:ty ),* ) => {
        $(
            impl LeBytes for $t {
                type Bytes = [u8; std::mem::size_of::<$t>()];
                const WGSL_NAME: &'static str = stringify!($t);

                fn to_le_bytes(self) -> Self::Bytes {
                    self.to_le_bytes()
                }

                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    todo!()
                }
            }
        )*
    }
}

impl_to_le_bytes!(f32, i32, u32);
