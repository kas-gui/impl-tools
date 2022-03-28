// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License in the LICENSE-APACHE file or at:
//     https://www.apache.org/licenses/LICENSE-2.0

use crate::fields::Fields;
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};
use proc_macro_error::emit_error;
use quote::quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Brace, Comma, Semi};
use syn::{
    Attribute, FieldsNamed, GenericParam, Generics, Ident, ItemImpl, Path, Result, Token, Type,
    Variant, Visibility,
};

/// Attribute rule for [`Scope`]
///
/// Rules are matched via a path, e.g. `&["foo"]` matches `foo` and
/// `&["", "foo", "bar"]` matches `::foo::bar`.
///
/// Such rules are used to expand attributes within an `impl_scope!`.
pub trait ScopeAttr {
    /// Attribute path
    ///
    /// Rules are matched via a path, e.g. `&["foo"]` matches `foo` and
    /// `&["", "foo", "bar"]` matches `::foo::bar`.
    ///
    /// Note that we cannot use standard path resolution, so we match only a
    /// single path, as defined.
    fn path(&self) -> &'static [&'static str];

    /// Function type of [`ScopeAttr`] rule
    ///
    /// Input arguments:
    ///
    /// -   `args`: attribute arguments. As with `#[proc_macro_attribute]`
    ///     macros, this is either empty or the contents of a single grouping token
    ///     (`()`, `{}` or `[]`).
    /// -   `span`: the span of the whole attribute. Note that [`Span::call_site`]
    ///     will return the span of the whole `impl_scope!` macro call. Use this
    ///     instead when reporting errors on the attribute.
    /// -   `scope`: mutable reference to the implementation scope. Usually
    ///     an attribute rule function will read data from the scope and append its
    ///     output to [`Scope::generated`].
    fn apply(&self, args: TokenStream, span: Span, scope: &mut Scope) -> Result<()>;
}

/// Content of items supported by [`Scope`] that are not common to all variants
#[derive(Debug)]
pub enum ScopeItem {
    /// A [`syn::ItemEnum`], minus common parts
    Enum {
        /// `enum`
        token: Token![enum],
        /// `{ ... }`
        brace: Brace,
        /// Variants of enum
        variants: Punctuated<Variant, Comma>,
    },
    /// A [`syn::ItemStruct`], minus common parts
    ///
    /// Uses custom [`Fields`], supporting field initializers.
    Struct {
        /// `struct`
        token: Token![struct],
        /// Fields of struct
        fields: Fields,
    },
    /// A [`syn::ItemType`], minus common parts
    Type {
        /// `type`
        token: Token![type],
        /// `=`
        eq_token: Token![=],
        /// Target type
        ty: Box<Type>,
    },
    /// A [`syn::ItemUnion`], minus common parts
    Union {
        /// `union`
        token: Token![union],
        /// Fields of union
        fields: FieldsNamed,
    },
}

impl ScopeItem {
    /// Take span of `enum`/`struct`/`type`/`union` token
    pub fn token_span(&self) -> Span {
        match self {
            ScopeItem::Enum { token, .. } => token.span,
            ScopeItem::Struct { token, .. } => token.span,
            ScopeItem::Type { token, .. } => token.span,
            ScopeItem::Union { token, .. } => token.span,
        }
    }
}

/// Contents of `impl_scope!`
///
/// `impl_scope!` input consists of one item (an `enum`, `struct`, `type` alias
/// or `union`) followed by any number of implementations, and is parsed into
/// this struct.
///
/// On its own, `impl_scope!` provides `impl Self` syntax: generics of the type
/// are attached to the implementation automatically.
///
/// The secondary utility of `impl_scope!` is to allow attribute expansion
/// within itself via [`ScopeAttr`] rules. These rules may read the type item
/// (which may include field initializers in the case of a struct), read
/// accompanying implementations, and even modify them.
#[derive(Debug)]
pub struct Scope {
    /// Outer attributes on the item
    pub attrs: Vec<Attribute>,
    /// Optional `pub`, etc.
    pub vis: Visibility,
    /// Item identifier
    pub ident: Ident,
    /// Item generics
    pub generics: Generics,
    /// The item
    pub item: ScopeItem,
    /// Trailing semicolon (type alias and unit struct only)
    pub semi: Option<Semi>,
    /// Implementation items
    pub impls: Vec<ItemImpl>,
    /// Output of [`ScopeAttr`] rules
    ///
    /// This does not contain any content from input, only content generated
    /// from [`ScopeAttr`] rules. It is appended to output as an item (usually
    /// a [`syn::ImplItem`]), after [`Self::impls`] items.
    pub generated: Vec<TokenStream>,
}

impl Scope {
    /// Apply attribute rules
    ///
    /// The supplied `rules` are applied in the order of definition, and their
    /// attributes removed from the item.
    pub fn apply_attrs(&mut self, rules: &[&dyn ScopeAttr]) {
        fn matches(p: &Path, mut q: &[&str]) -> bool {
            assert!(!q.is_empty());
            if p.leading_colon.is_some() {
                if !q[0].is_empty() {
                    return false;
                }
                q = &q[1..];
            }

            if p.segments.len() != q.len() {
                return false;
            }

            for (x, y) in p.segments.iter().zip(q.iter()) {
                if x.ident != y || !x.arguments.is_empty() {
                    return false;
                }
            }

            true
        }

        let mut i = 0;
        while i < self.attrs.len() {
            for rule in rules {
                if matches(&self.attrs[i].path, rule.path()) {
                    let attr = self.attrs.remove(i);
                    let span = attr.span();

                    // Emulate #[proc_macro]: attr must contain 0 or 1 group
                    let mut iter = attr.tokens.into_iter();
                    let mut tokens = TokenStream::new();
                    if let Some(tree) = iter.next() {
                        match tree {
                            TokenTree::Group(group) if group.delimiter() != Delimiter::None => {
                                tokens = group.stream();
                            }
                            _ => {
                                emit_error!(tree, "expected one of `(`, `::`, `[`, `]`, or `{`");
                                continue;
                            }
                        }
                    }
                    if let Some(tree) = iter.next() {
                        emit_error!(tree, "expected `]`");
                        continue;
                    }

                    if let Err(err) = rule.apply(tokens, span, self) {
                        emit_error!(err.span(), "{}", err);
                    }
                    continue;
                }
            }

            i += 1;
        }
    }

    /// Generate the result
    pub fn generate(self) -> TokenStream {
        quote! { #self }
    }
}

mod parsing {
    use super::*;
    use crate::fields::parsing::data_struct;
    use syn::parse::{Parse, ParseStream};
    use syn::spanned::Spanned;
    use syn::{
        braced, bracketed, parse_quote, AttrStyle, Error, Field, Lifetime, Path, TypePath,
        WhereClause,
    };

    impl Parse for Scope {
        fn parse(input: ParseStream) -> Result<Self> {
            let attrs = input.call(Attribute::parse_outer)?;
            let vis = input.parse::<Visibility>()?;

            enum Token {
                Enum(Token![enum]),
                Struct(Token![struct]),
                Type(Token![type]),
                Union(Token![union]),
            }
            let lookahead = input.lookahead1();
            let token;
            if lookahead.peek(Token![enum]) {
                token = Token::Enum(input.parse()?);
            } else if lookahead.peek(Token![struct]) {
                token = Token::Struct(input.parse()?);
            } else if lookahead.peek(Token![type]) {
                token = Token::Type(input.parse()?);
            } else if lookahead.peek(Token![union]) {
                token = Token::Union(input.parse()?);
            } else {
                return Err(lookahead.error());
            }

            let ident = input.parse::<Ident>()?;
            let mut generics = input.parse::<Generics>()?;

            let item;
            let mut semi = None;
            match token {
                Token::Enum(token) => {
                    let (wc, brace, variants) = data_enum(input)?;
                    generics.where_clause = wc;
                    item = ScopeItem::Enum {
                        token,
                        brace,
                        variants,
                    };
                }
                Token::Struct(token) => {
                    let (wc, fields, semi_token) = data_struct(input)?;
                    generics.where_clause = wc;
                    semi = semi_token;
                    item = ScopeItem::Struct { token, fields };
                }
                Token::Type(token) => {
                    let eq_token = input.parse()?;
                    let ty = input.parse()?;
                    let semi_token = input.parse()?;
                    semi = Some(semi_token);
                    item = ScopeItem::Type {
                        token,
                        eq_token,
                        ty,
                    };
                }
                Token::Union(token) => {
                    let (wc, fields) = data_union(input)?;
                    generics.where_clause = wc;
                    item = ScopeItem::Union { token, fields };
                }
            }

            let mut impls = Vec::new();
            while !input.is_empty() {
                impls.push(parse_impl(&generics, &ident, input)?);
            }

            Ok(Scope {
                attrs,
                vis,
                ident,
                generics,
                item,
                semi,
                impls,
                generated: vec![],
            })
        }
    }

    fn parse_impl(
        in_generics: &Generics,
        in_ident: &Ident,
        input: ParseStream,
    ) -> Result<ItemImpl> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let defaultness: Option<Token![default]> = input.parse()?;
        let unsafety: Option<Token![unsafe]> = input.parse()?;
        let impl_token: Token![impl] = input.parse()?;

        let has_generics = input.peek(Token![<])
            && (input.peek2(Token![>])
                || input.peek2(Token![#])
                || (input.peek2(Ident) || input.peek2(Lifetime))
                    && (input.peek3(Token![:])
                        || input.peek3(Token![,])
                        || input.peek3(Token![>])
                        || input.peek3(Token![=]))
                || input.peek2(Token![const]));
        let mut generics: Generics = if has_generics {
            input.parse()?
        } else {
            Generics::default()
        };

        let mut first_ty: Type = input.parse()?;
        let mut self_ty: Type;
        let trait_;

        let is_impl_for = input.peek(Token![for]);
        if is_impl_for {
            let for_token: Token![for] = input.parse()?;
            let mut first_ty_ref = &first_ty;
            while let Type::Group(ty) = first_ty_ref {
                first_ty_ref = &ty.elem;
            }
            if let Type::Path(_) = first_ty_ref {
                while let Type::Group(ty) = first_ty {
                    first_ty = *ty.elem;
                }
                if let Type::Path(TypePath { qself: None, path }) = first_ty {
                    trait_ = Some((None, path, for_token));
                } else {
                    unreachable!();
                }
            } else {
                return Err(Error::new(for_token.span, "for without target trait"));
            }
            self_ty = input.parse()?;
        } else {
            trait_ = None;
            self_ty = first_ty;
        }

        generics.where_clause = input.parse()?;

        if self_ty == parse_quote! { Self } {
            let (_, ty_generics, _) = in_generics.split_for_impl();
            self_ty = parse_quote! { #in_ident #ty_generics };
            extend_generics(&mut generics, in_generics);
        } else if !matches!(self_ty, Type::Path(TypePath {
                qself: None,
                path: Path {
                    leading_colon: None,
                    ref segments,
                }
            }) if segments.len() == 1 && segments.first().unwrap().ident == *in_ident)
        {
            return Err(Error::new(
                self_ty.span(),
                format!(
                    "expected `Self` or `{0}` or `{0}<...>` or `Trait for Self`, etc",
                    in_ident
                ),
            ));
        }

        let content;
        let brace_token = braced!(content in input);
        parse_attrs_inner(&content, &mut attrs)?;

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }

        Ok(ItemImpl {
            attrs,
            defaultness,
            unsafety,
            impl_token,
            generics,
            trait_,
            self_ty: Box::new(self_ty),
            brace_token,
            items,
        })
    }

    pub fn data_enum(
        input: ParseStream,
    ) -> Result<(Option<WhereClause>, Brace, Punctuated<Variant, Token![,]>)> {
        let where_clause = input.parse()?;

        let content;
        let brace = braced!(content in input);
        let variants = content.parse_terminated(Variant::parse)?;

        Ok((where_clause, brace, variants))
    }

    pub fn data_union(input: ParseStream) -> Result<(Option<WhereClause>, FieldsNamed)> {
        let where_clause = input.parse()?;
        let fields = parse_braced(input)?;
        Ok((where_clause, fields))
    }

    pub(crate) fn parse_braced(input: ParseStream) -> Result<FieldsNamed> {
        let content;
        let brace_token = braced!(content in input);
        let named = content.parse_terminated(Field::parse_named)?;
        Ok(FieldsNamed { brace_token, named })
    }

    fn parse_attrs_inner(input: ParseStream, attrs: &mut Vec<Attribute>) -> Result<()> {
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            let pound_token = input.parse()?;
            let style = AttrStyle::Inner(input.parse()?);
            let content;
            let bracket_token = bracketed!(content in input);
            let path = content.call(Path::parse_mod_style)?;
            let tokens = content.parse()?;
            attrs.push(Attribute {
                pound_token,
                style,
                bracket_token,
                path,
                tokens,
            });
        }
        Ok(())
    }
}

mod printing {
    use super::*;
    use quote::{ToTokens, TokenStreamExt};

    impl ToTokens for Scope {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            tokens.append_all(self.attrs.iter());
            self.vis.to_tokens(tokens);
            match &self.item {
                ScopeItem::Enum { token, .. } => token.to_tokens(tokens),
                ScopeItem::Struct { token, .. } => token.to_tokens(tokens),
                ScopeItem::Type { token, .. } => token.to_tokens(tokens),
                ScopeItem::Union { token, .. } => token.to_tokens(tokens),
            }
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            match &self.item {
                ScopeItem::Enum {
                    brace, variants, ..
                } => {
                    self.generics.where_clause.to_tokens(tokens);
                    brace.surround(tokens, |tokens| {
                        variants.to_tokens(tokens);
                    });
                }
                ScopeItem::Struct { fields, .. } => match fields {
                    Fields::Named(fields) => {
                        self.generics.where_clause.to_tokens(tokens);
                        fields.to_tokens(tokens);
                    }
                    Fields::Unnamed(fields) => {
                        fields.to_tokens(tokens);
                        self.generics.where_clause.to_tokens(tokens);
                    }
                    Fields::Unit => {
                        self.generics.where_clause.to_tokens(tokens);
                    }
                },
                ScopeItem::Type { eq_token, ty, .. } => {
                    self.generics.where_clause.to_tokens(tokens);
                    eq_token.to_tokens(tokens);
                    ty.to_tokens(tokens);
                }
                ScopeItem::Union { fields, .. } => {
                    self.generics.where_clause.to_tokens(tokens);
                    fields.to_tokens(tokens);
                }
            }
            if let Some(semi) = self.semi.as_ref() {
                semi.to_tokens(tokens);
            }

            tokens.append_all(self.impls.iter());
            tokens.append_all(self.generated.iter());
        }
    }
}

// Support impls on Self by replacing name and summing generics
fn extend_generics(generics: &mut Generics, in_generics: &Generics) {
    if generics.lt_token.is_none() {
        debug_assert!(generics.params.is_empty());
        debug_assert!(generics.gt_token.is_none());
        generics.lt_token = in_generics.lt_token;
        generics.params = in_generics.params.clone();
        generics.gt_token = in_generics.gt_token;
    } else if in_generics.lt_token.is_none() {
        debug_assert!(in_generics.params.is_empty());
        debug_assert!(in_generics.gt_token.is_none());
    } else {
        if !generics.params.empty_or_trailing() {
            generics.params.push_punct(Default::default());
        }
        generics
            .params
            .extend(in_generics.params.clone().into_pairs());
    }

    // Strip defaults which are legal on the struct but not on impls
    for param in &mut generics.params {
        match param {
            GenericParam::Type(p) => {
                p.eq_token = None;
                p.default = None;
            }
            GenericParam::Lifetime(_) => (),
            GenericParam::Const(p) => {
                p.eq_token = None;
                p.default = None;
            }
        }
    }

    if let Some(ref mut clause1) = generics.where_clause {
        if let Some(ref clause2) = in_generics.where_clause {
            if !clause1.predicates.empty_or_trailing() {
                clause1.predicates.push_punct(Default::default());
            }
            clause1
                .predicates
                .extend(clause2.predicates.clone().into_pairs());
        }
    } else {
        generics.where_clause = in_generics.where_clause.clone();
    }
}
