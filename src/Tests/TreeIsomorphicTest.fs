module TreeIsomorphicTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open Lines


// Isomorphic subtrees have isomorphic renderings. => Isomorphic
[<Property>]
let ``Isomorphic subtrees have isomorphic renderings`` (t: Tree<'a*float>) =
    // 1. get absolute position of all subtree => absolutePositionTree
    // 2. get the list of their position
    // 3. check the position is identical
    let positionTree = absolutePositionTree t

    true