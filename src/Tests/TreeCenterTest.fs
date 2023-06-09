// #############################################
// # Authors: Bingkun                          #
// # Date: June 6th                            #
// # Last edit: June 7th                       #
// #############################################
module Tests.TreeCenterTest

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open Lines


let rec checkParentCentered (Node((_, pos), subtrees): Tree<(char * float)>) =
        match subtrees with
        | [] -> true
        | _ -> 
            let childPos = subtrees |> List.map (fun (Node((_, childPos), _)) -> childPos + pos)
            let minPos = childPos |> List.min
            let maxPos = childPos |> List.max
            let avgPos = (minPos + maxPos) / 2.0
            abs(avgPos - pos) < 0.000001 && (subtrees |> List.forall checkParentCentered)

[<TestCase>]
let ``test no subtree`` () = 
    let testTree = Node ('a', [])
    let testTree' = design testTree
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with only one node`` () = 
    let testTree = Node ('a', [Node ('a', []);])
    let testTree' = design testTree
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with even nodes`` () = 
    let testTree = Node ('a', [Node ('a', []);Node ('a', [])])
    let testTree' = design testTree
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with odd nodes`` () = 
    let testTree = Node ('a', [Node ('a', []);Node ('a', []);Node ('a', [])])
    let testTree' = design testTree
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``a simple three depths tree`` () = 
    let testTree = Node('a',[Node ('a', [Node ('a', []); Node ('a', [])]); Node ('a', [Node ('a', [])]);Node ('a', [])])
    let testTree' = design testTree
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<Property>]
let ``All parent should be centered above its immediate children`` (t: Tree<char>) = 
    let testTree = design t
    testTree |> checkParentCentered