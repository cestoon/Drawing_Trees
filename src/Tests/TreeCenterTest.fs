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
            let avgPos = childPos |> List.average
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
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<Property>]
let ``All parent should be centered above its immediate children`` (t: Tree<char>) = 
    let testTree = design t
    printfn "1. %A" testTree 
    testTree |> checkParentCentered