use syn::parse::{Parse, ParseBuffer};
use syn::{Ident, Token};
pub struct SeqInput {
    pub ident: Ident,
    pub in_token: Token![in],
    pub range_start: syn::LitInt,
    pub dot_token: Token![..],
    pub range_end: syn::LitInt,
    pub block: syn::Block,
}

impl Parse for SeqInput {
    fn parse(input: &ParseBuffer) -> Result<Self, syn::Error> {
        let ident = input.parse()?;
        let in_token = input.parse()?;
        let range_start = input.parse()?;
        let dot_token = input.parse()?;
        let range_end = input.parse()?;
        let block = input.parse()?;
        Ok(SeqInput {
            ident,
            in_token,
            range_start,
            dot_token,
            range_end,
            block,
        })
    }
}
