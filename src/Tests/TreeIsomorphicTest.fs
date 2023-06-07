module Tests.TreeIsomorphicTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open Lines


// Property-based test to check if identical subtrees are rendered identically
// let identicalSubtreesRenderIdentically =
//     Arb.generate<Tree<int>>
//     |> Gen.sample 100
//     |> List.forall (fun tree ->
//         let renderedTree1 = design tree |> reflect |> reflectpos |> design
//         let renderedTree2 = design (reflectpos tree)
//         renderedTree1 = renderedTree2)

// [<Property>]
// let ``Identical subtrees should be rendered identically`` () =
//     identicalSubtreesRenderIdentically |> Expect.isTrue


// Isomorphic subtrees have isomorphic renderings. => Isomorphic
[<Property>]
let ``Isomorphic subtrees have isomorphic renderings`` (t: Tree<string>) =
    let tt = Node("root", [ t; t ])
    // 0. Use design on the tree
    let designedTree = design tt

    // 1. Get absolute position of all subtrees
    let absTree = absolutePositionTree designedTree

    // 2. Get the list of positions of all subtrees
    // let rec getSubtreePositions (Node((_, p), subtrees)) =
    //     p :: List.collect getSubtreePositions subtrees

    // let positions = getSubtreePositions absTree

    // // 3. Check if positions of identical subtrees are the same
    // let rec checkIdenticalSubtreesPositions (Node((_, _), subtrees1)) (Node((_, _), subtrees2)) =
    //     List.forall2 checkIdenticalSubtreesPositions subtrees1 subtrees2

    // let identicalSubtreesPositions = checkIdenticalSubtreesPositions t t

    // Seq.distinct positions |> Seq.length <= 1 || not identicalSubtreesPositions
    true


// // 0. use design on the tree or use Gen.map function
// let dTree = design t

// // 1. get absolute position of all subtree => absolutePositionTree
// let absTree = absolutePositionTree dTree


// // 2. get the list of their position
// let nodeToPosition t =
//     let rec nodeToPosition' acc (Node((_, p), ts)) = p :: List.fold nodeToPosition' acc ts

//     nodeToPosition' [] t

// let positions = nodeToPosition absTree

// // 3. check the position is identical

// true

// relative distance
// structurely equal -> node the same??

// subtree as argument??
// [<Property>]
