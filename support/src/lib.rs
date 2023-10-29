use proc_macro::TokenStream;
use quote::quote;

/// Derive the `Indexable` trait for an enum
/// The trait is used to get the index of an enum variant
/// even when no discriminant is available
#[proc_macro_derive(Indexable)]
pub fn indexable_derive(input: TokenStream) -> TokenStream {
    let ast: syn::ItemEnum = syn::parse(input).unwrap();
    let enum_name = &ast.ident;
    let vars = ast.variants;
    let mut res = "impl".to_string();
    let letters = ["T", "U", "V", "W", "X", "Y", "Z"];
    let mut generic_str = "".to_string();
    if !ast.generics.params.is_empty() {
        generic_str += "<";
        for i in ast.generics.params.iter().enumerate().map(|(i, _)| i) {
            if i != 0 {
                generic_str += ", ";
            }
            generic_str += letters[i];
        }
        generic_str += ">";
    }
    res += &format!("{generic_str} {enum_name}{generic_str}");

    res += r#"{
        pub fn index(&self) -> usize {
            match self {
    "#;
    for (index, v) in vars.iter().enumerate() {
        res += &format!(
            "{enum_name}::{variant} {{..}} => {index},",
            variant = v.ident,
        );
    }
    res += r#"
            }
        }
    }"#;
    res.parse().unwrap()
}

#[proc_macro]
pub fn array_pcfg(item: TokenStream) -> TokenStream {
    let typ: syn::TypeArray = syn::parse(item).unwrap();
    let t = typ.clone();
    let len = typ.len;
    quote! {
        impl PCFG for #t {
            fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
                out.push(PSpace(self.to_vec()));
                out
            }

            fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
            where
                Self: Sized,
            {
                let res = input.pop().unwrap().0.try_into().unwrap();
                (res, input)
            }

            fn uniform() -> Self {
                #[allow(clippy::cast_precision_loss)]
                std::iter::repeat((1.0 / Self::COUNT as f64).ln())
                    .take(Self::COUNT)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap()
            }

            const COUNT: usize = #len;

        }
    }
    .try_into()
    .unwrap()
}
