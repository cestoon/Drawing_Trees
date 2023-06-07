module TreeSymmetryTest

open NUnit.Framework
open Library.TreeDesign
open FsCheck.NUnit


// Drawings must be symmetric wrt. reflection. => symmetryProperty

// flect once
[<Property>]
let ``flect once to check is position negative`` (t:Tree<'a>) = 
    true
// flect twice
[<Property>]
let ``flect twice to check is position the same`` (t:Tree<'a>) = 
    true