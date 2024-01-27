use syn::Path;

pub enum Extrapolate {
    Default,
    Function(Path),
}
