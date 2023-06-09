module TreeDistanceTest

open NUnit.Framework
open Library.TreeDesign
open FsCheck.NUnit

// • Two nodes at the same level are placed a given distance apart. => minimum_distance_check
// 1. no duplicate position for all node
// 2. all distance >= minimum distance <= absolute pos

let rec collectNodes (Node((v, (x, y)), subtrees): Tree<('a * (float * float))>) =
    match subtrees with
    | [] -> [(y, x)]
    | _ -> (y, x) :: (List.collect collectNodes subtrees)

[<Property>]
let ``no_duplicate_position_for_all_node`` (t: Tree<char * (float * float)>) =
    let absDt = absDesign t
    let nodes = collectNodes absDt
    let distinctNodes = List.distinct nodes

    nodes.Length = distinctNodes.Length

let checkDistance nodes = 
        let sortedNodes = nodes |> Array.sortBy snd
        sortedNodes
        |> Array.pairwise
        |> Array.forall (fun ((_, x1), (_, x2)) -> x2 - x1 >= 1.0)
    
[<Property>]
let ``all_distance_of_node_is_bigger_than_1.0`` (t:Tree<char>) = 
    let absDt = absDesign t
    let nodesAtEachLevel = collectNodes absDt |> List.groupBy fst
    let result =
        nodesAtEachLevel
        |> List.map snd
        |> List.forall (fun nodes -> nodes |> Array.ofList |> checkDistance)
    if result then
        printfn "Test passed."
    result