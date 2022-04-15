use proc_macro::TokenStream;

mod seqinput;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _ = input;
    let input = syn::parse_macro_input!(input as seqinput::SeqInput);
    match do_expand(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
fn do_expand(input: &seqinput::SeqInput) -> syn::Result<proc_macro2::TokenStream> {
    Ok(proc_macro2::TokenStream::new())
}
