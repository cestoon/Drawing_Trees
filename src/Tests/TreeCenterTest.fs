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
            let childPos = subtrees |> List.map (fun (Node((_, pos), _)) -> pos)
            let avgChildPos = childPos |> List.average
            printfn "avgChildPos%f" avgChildPos
            printfn "pos%f" pos
            printfn "pos%f" (avgChildPos - pos)
            abs(avgChildPos - pos) < 0.000001 && (subtrees |> List.forall checkParentCentered)

[<TestCase>]
let ``test no subtree`` () = 
    let testTree = Node ('a', [])
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with only one node`` () = 
    let testTree = Node ('a', [Node ('a', []);])
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with two node`` () = 
    let testTree = Node ('a', [Node ('a', []);Node ('a', [])])
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)

[<TestCase>]
let ``test subtree with three node`` () = 
    let testTree = Node ('a', [Node ('a', []);Node ('a', []);Node ('a', [])])
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)


[<Property>]
let ``tt`` (t: Tree<char>) = 
    let testTree = design t
    printfn "1. %A" testTree 
    testTree |> checkParentCentered
    
[<TestCase>]
let ``test3`` () = 
    let testTree = Node ('a', [Node ('a', [Node ('a', []);Node ('a', [])]); Node ('a', [])])
    printfn "1. %A" testTree 
    let testTree' = design testTree
    printfn "2. %A" testTree'
    let result = testTree' |> checkParentCentered
    Assert.IsTrue(result)
