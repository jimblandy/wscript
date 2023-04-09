//! Producing a WGSL type name from a Naga `TypeInner`.

use naga::{Handle, Module, Type, TypeInner};
use std::fmt;

/// A wrapper for types that can be formatted as WGSL source code.
///
/// If a value `v` of some type `T` has a natural representation as WGSL source,
/// then `Wgsl(v)` implements `std::fmt::Display` by formatting `v` as WGSL
/// source.
///
/// Types like `Handle<Type>` and `TypeInner` need a `Module` to be interpreted
/// properly, so for those we use a `(v, module)` pair as `T`.
pub struct Wgsl<T>(pub T);

/// Format a `naga::Handle<naga::Type>` as WGSL source code.
///
/// This produces a WGSL `type_specifier`, not a `global_decl`. That
/// is, for a `struct` type `S`, this would return `"struct S"`, not a
/// full declaration of `S` like `"struct S { a: i32 }"`.
impl fmt::Display for Wgsl<(Handle<Type>, &'_ Module)> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Wgsl((handle, module)) = *self;
        let ty = &module.types[handle];

        if let Some(ref name) = ty.name {
            f.write_str(name)
        } else {
            Wgsl((&ty.inner, module)).fmt(f)
        }
    }
}

impl<'m> fmt::Display for Wgsl<(&'m TypeInner, &'m Module)> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeInner as Ti;

        let Wgsl((inner, module)) = *self;
        match *inner {
            Ti::Scalar { kind, width } => Scalar { kind, width }.fmt(f),
            Ti::Vector { size, kind, width } => Vector { size, kind, width }.fmt(f),
            Ti::Matrix {
                columns,
                rows,
                width,
            } => {
                let columns = columns as u8;
                let rows = rows as u8;
                let element = Scalar {
                    kind: naga::ScalarKind::Float,
                    width,
                };
                write!(f, "mat{columns}x{rows}<{element}>")
            }
            Ti::Atomic { kind, width } => {
                let element = Scalar { kind, width };
                write!(f, "atomic<{element}>")
            }
            Ti::Pointer { base, space } => Pointer {
                base: Wgsl((base, module)),
                space,
            }
            .fmt(f),
            Ti::ValuePointer {
                size,
                kind,
                width,
                space,
            } => match size {
                Some(size) => Pointer {
                    base: Vector { size, kind, width },
                    space,
                }
                .fmt(f),
                None => Pointer {
                    base: Scalar { kind, width },
                    space,
                }
                .fmt(f),
            },
            Ti::Array {
                base,
                size,
                stride: _,
            } => {
                let base = Wgsl((base, module));
                match size.to_indexable_length(module).unwrap() {
                    naga::proc::index::IndexableLength::Known(len) => {
                        write!(f, "array<{base}, {len}>")
                    }
                    naga::proc::index::IndexableLength::Dynamic => {
                        write!(f, "array<{base}>")
                    }
                }
            }
            Ti::Struct { .. } => {
                // The name is only available in `naga::Type`, so we shouldn't
                // be handling this here.
                write!(f, "struct <anonymous>")
            }
            Ti::Image {
                dim,
                arrayed,
                class,
            } => Image {
                dim,
                arrayed,
                class,
            }
            .fmt(f),
            Ti::Sampler { comparison } => {
                write!(f, "sampler{}", if comparison { "_comparison" } else { "" })
            }
            Ti::BindingArray { base, size } => {
                let base = Wgsl((base, module));
                match size.to_indexable_length(module).unwrap() {
                    naga::proc::index::IndexableLength::Known(len) => {
                        write!(f, "binding_array<{base}, {len}>")
                    }
                    naga::proc::index::IndexableLength::Dynamic => {
                        write!(f, "binding_array<{base}>")
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct Scalar {
    kind: naga::ScalarKind,
    width: naga::Bytes,
}

impl fmt::Display for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use naga::ScalarKind as Sk;
        match self.kind {
            Sk::Sint => write!(f, "i{}", self.width * 8),
            Sk::Uint => write!(f, "u{}", self.width * 8),
            Sk::Float => write!(f, "f{}", self.width * 8),
            Sk::Bool => f.write_str("bool"),
        }
    }
}

#[derive(Debug)]
struct Vector {
    size: naga::VectorSize,
    kind: naga::ScalarKind,
    width: naga::Bytes,
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let size = self.size as u8;
        let element = Scalar {
            kind: self.kind,
            width: self.width,
        };
        write!(f, "vec{size}<{element}>")
    }
}

struct Pointer<B> {
    base: B,
    space: naga::AddressSpace,
}

impl<B> fmt::Display for Pointer<B>
where
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Pointer { ref base, space } = *self;
        use naga::AddressSpace as As;
        let space_wgsl = Wgsl(space);
        match space {
            As::Storage { access } => {
                if access == naga::StorageAccess::LOAD {
                    write!(f, "ptr<{space_wgsl}, {base}>")
                } else {
                    let access = Wgsl(access);
                    write!(f, "ptr<{space_wgsl}, {base}, {access}>")
                }
            }
            As::Function
            | As::Private
            | As::WorkGroup
            | As::Uniform
            | As::Handle
            | As::PushConstant => {
                write!(f, "ptr<{space_wgsl}, {base}>")
            }
        }
    }
}

impl fmt::Display for Wgsl<naga::AddressSpace> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use naga::AddressSpace as As;
        f.write_str(match self.0 {
            As::Function => "function",
            As::Private => "private",
            As::WorkGroup => "workgroup",
            As::Uniform => "uniform",
            As::Storage { .. } => "storage",
            As::Handle => "handle",
            As::PushConstant => "pushconstant",
        })
    }
}

impl fmt::Display for Wgsl<naga::StorageAccess> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use naga::StorageAccess as Sa;
        f.write_str(match self.0 {
            Sa::LOAD => "read",
            Sa::STORE => "write",
            a if a == Sa::LOAD | Sa::STORE => "read_write",
            _ => panic!("Unexpected naga::StorageAccess: {:?}", self.0),
        })
    }
}

struct Image {
    dim: naga::ImageDimension,
    arrayed: bool,
    class: naga::ImageClass,
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let dim = match self.dim {
            naga::ImageDimension::D1 => "1d",
            naga::ImageDimension::D2 => "2d",
            naga::ImageDimension::D3 => "3d",
            naga::ImageDimension::Cube => "cube",
        };

        let arrayed = match self.arrayed {
            true => "_array",
            false => "",
        };

        fn multi_wgsl(multi: bool) -> &'static str {
            match multi {
                true => "_multisampled",
                false => "",
            }
        }

        match self.class {
            naga::ImageClass::Sampled { kind, multi } => {
                let multi = multi_wgsl(multi);
                let scalar = Scalar { kind, width: 4 };
                write!(f, "texture{multi}_{dim}{arrayed}<{scalar}>")
            }
            naga::ImageClass::Depth { multi } => {
                let multi = multi_wgsl(multi);
                write!(f, "texture_depth{multi}_{dim}{arrayed}")
            }
            naga::ImageClass::Storage { format, access } => {
                let format = Wgsl(format);
                let access = Wgsl(access);
                write!(f, "texture_storage_{dim}{arrayed}<{format}, {access}>")
            }
        }
    }
}

impl fmt::Display for Wgsl<naga::StorageFormat> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use naga::StorageFormat as Sf;
        f.write_str(match self.0 {
            Sf::R8Unorm => "r8unorm",
            Sf::R8Snorm => "r8snorm",
            Sf::R8Uint => "r8uint",
            Sf::R8Sint => "r8sint",
            Sf::R16Uint => "r16uint",
            Sf::R16Sint => "r16sint",
            Sf::R16Float => "r16float",
            Sf::Rg8Unorm => "rg8unorm",
            Sf::Rg8Snorm => "rg8snorm",
            Sf::Rg8Uint => "rg8uint",
            Sf::Rg8Sint => "rg8sint",
            Sf::R32Uint => "r32uint",
            Sf::R32Sint => "r32sint",
            Sf::R32Float => "r32float",
            Sf::Rg16Uint => "rg16uint",
            Sf::Rg16Sint => "rg16sint",
            Sf::Rg16Float => "rg16float",
            Sf::Rgba8Unorm => "rgba8unorm",
            Sf::Rgba8Snorm => "rgba8snorm",
            Sf::Rgba8Uint => "rgba8uint",
            Sf::Rgba8Sint => "rgba8sint",
            Sf::Rgb10a2Unorm => "rgb10a2unorm",
            Sf::Rg11b10Float => "rg11b10float",
            Sf::Rg32Uint => "rg32uint",
            Sf::Rg32Sint => "rg32sint",
            Sf::Rg32Float => "rg32float",
            Sf::Rgba16Uint => "rgba16uint",
            Sf::Rgba16Sint => "rgba16sint",
            Sf::Rgba16Float => "rgba16float",
            Sf::Rgba32Uint => "rgba32uint",
            Sf::Rgba32Sint => "rgba32sint",
            Sf::Rgba32Float => "rgba32float",
        })
    }
}
