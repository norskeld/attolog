module Attolog.Prelude

/// Dereferences a mutable reference cell.
let (!) (r: ref<'a>) = r.Value

/// Mutates a reference cell.
let (:=) (r: ref<'a>) (v: 'a) = r.Value <- v
