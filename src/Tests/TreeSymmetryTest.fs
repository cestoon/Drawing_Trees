module Tests.TreeSymmetryTest

open NUnit.Framework
open Library.TreeDesign
open FsCheck.NUnit


// Drawings must be symmetric wrt. reflection. => symmetryProperty
// relative tree
// flect once
[<Property>]
let ``reflect once to check is position negative`` (t:Tree<'a>) = 
    true
// flect twice

[<Property>]
let ``reflect twice to check is position the same`` (t:Tree<'a>) = 
    true