module TreeCenterTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open TGenerators

// • A parent should be centered above its immediate children. => centeringProperty
[<Property>]
let ``Design generates valid trees`` (t: Tree<int>) =
    let result = design t
    // some test here
    printfn "%A" result // 打印 result 的值

    true

let ``Design generates valid char trees`` (t: Tree<char>) =
    let result = design t
    // some test here
    true

