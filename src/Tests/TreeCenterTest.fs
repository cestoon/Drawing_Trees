module TreeCenterTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign

[<Property>]
let ``Design generates valid trees`` (t: Tree<int>) =
    let result = design t
    // some test here
    true
[<Property>]
let ``Design generates valid char trees`` (t: Tree<char>) =
    let result = design t
    // some test here
    printfn "%A" result
    true
[<Property>]
let ``A parent should be centered above its immediate children`` (t:Tree<'a>) = 
    true
    