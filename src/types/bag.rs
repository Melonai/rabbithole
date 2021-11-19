use std::{cell::RefCell, collections::HashMap};

use super::{RefValueMap, Type, TypeVariant};

#[derive(Clone)]
pub struct TypeBag {
    global_associated_values: &'static GlobalAssociatedValues,
    atomic_types: AtomicTypes,
}

impl TypeBag {
    pub fn new() -> Self {
        // Hehe don't mind me
        let leaked_values: &'static GlobalAssociatedValues =
            Box::leak(Box::new(GlobalAssociatedValues::new()));
        let atomic_types = AtomicTypes::new(&leaked_values);

        TypeBag {
            global_associated_values: leaked_values,
            atomic_types,
        }
    }

    pub fn create_type(&self, variant: TypeVariant) -> Type {
        let global_associated_values_for_type = match variant {
            TypeVariant::Fn { .. } => &self.global_associated_values.fn_values,
            TypeVariant::Array(_) => &self.global_associated_values.array_values,
            TypeVariant::Tuple(_) => &self.global_associated_values.tuple_values,
            TypeVariant::Data { .. } => &self.global_associated_values.data_values,
            TypeVariant::Str
            | TypeVariant::Int
            | TypeVariant::Float
            | TypeVariant::Bool
            | TypeVariant::Void => panic!("There should only be one type {:?}.", variant),
            TypeVariant::Face(_) | TypeVariant::Var(_) => todo!("Implement abstract types."),
        };

        Type {
            variant: Box::new(variant),
            global_associated_values: global_associated_values_for_type,
        }
    }

    pub fn str(&self) -> Type {
        self.atomic_types.str_type.clone()
    }

    pub fn int(&self) -> Type {
        self.atomic_types.int_type.clone()
    }

    pub fn float(&self) -> Type {
        self.atomic_types.float_type.clone()
    }

    pub fn bool(&self) -> Type {
        self.atomic_types.bool_type.clone()
    }

    pub fn void(&self) -> Type {
        self.atomic_types.void_type.clone()
    }
}

struct GlobalAssociatedValues {
    str_values: RefValueMap,
    int_values: RefValueMap,
    float_values: RefValueMap,
    bool_values: RefValueMap,
    void_values: RefValueMap,
    fn_values: RefValueMap,
    array_values: RefValueMap,
    tuple_values: RefValueMap,
    data_values: RefValueMap,
}

impl GlobalAssociatedValues {
    fn new() -> Self {
        GlobalAssociatedValues {
            str_values: Self::create_value_map(),
            int_values: Self::create_value_map(),
            float_values: Self::create_value_map(),
            bool_values: Self::create_value_map(),
            void_values: Self::create_value_map(),
            fn_values: Self::create_value_map(),
            array_values: Self::create_value_map(),
            tuple_values: Self::create_value_map(),
            data_values: Self::create_value_map(),
        }
    }

    fn create_value_map() -> RefValueMap {
        RefCell::new(HashMap::new())
    }
}

#[derive(Clone)]
struct AtomicTypes {
    str_type: Type,
    int_type: Type,
    float_type: Type,
    bool_type: Type,
    void_type: Type,
}

impl AtomicTypes {
    fn new(values: &'static GlobalAssociatedValues) -> Self {
        AtomicTypes {
            str_type: Self::create_single_type(TypeVariant::Str, &values.str_values),
            int_type: Self::create_single_type(TypeVariant::Int, &values.int_values),
            float_type: Self::create_single_type(TypeVariant::Float, &values.float_values),
            bool_type: Self::create_single_type(TypeVariant::Bool, &values.bool_values),
            void_type: Self::create_single_type(TypeVariant::Void, &values.void_values),
        }
    }

    fn create_single_type(variant: TypeVariant, values: &'static RefValueMap) -> Type {
        Type {
            variant: Box::new(variant),
            global_associated_values: values,
        }
    }
}
