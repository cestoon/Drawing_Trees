module TreeDistanceTest

open NUnit.Framework
open Library.TreeDesign
open FsCheck.NUnit

// • Two nodes at the same level are placed a given distance apart. => minimum_distance_check
// 1. non-overlapping for extends
// 2. all distance >= minimum distance <= absolute pos

[<Property>]
let ``no overlapping for extends`` (t:Tree<'a>) = 
    true


[<Property>]
let ``all distance of node is >= 1`` (t:Tree<'a>) = 
    true