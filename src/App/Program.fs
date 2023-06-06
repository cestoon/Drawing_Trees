open System.IO
open Library.TreeDesign

open Plotly.NET
open Plotly.NET.LayoutObjects

open Lines

let t1 = Node("A", [])
let t2 = Node("B", [])

let t3 = Node("C", [ t1; t2 ])
let t4 = Node("D", [])

let t = Node("Root", [ t3; t4 ])


let dTree = design t
let absTree = absolutePositionTree dTree


let points = nodeToPoint absTree
let lines = treeLines absTree

let mirroredXAxis' =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false)

let mirroredYAxis' =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false)


points @ lines
|> Chart.combine
|> Chart.withXAxis mirroredXAxis'
|> Chart.withYAxis mirroredYAxis'
|> Chart.show

//################################################
//#     Code for main function starts below      #
//################################################
let mirroredXAxis =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false, Visible = false)

let mirroredYAxis =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false, Visible = false)

let getSavePath() =
    let folder = "./out"
    let path = $"{folder}/Tree-{System.DateTime.Now.ToFileTime()}.html"
    
    if not (Directory.Exists folder) then Directory.CreateDirectory folder |> ignore
    path

let parseTree = function
    | Some s -> t //TODO: Insert actual implementation here if necessary
    | None -> ExampleTrees.getRandomTree() 

[<EntryPoint>]
let main argv = 
    argv
    |> Array.tryHead
    |> parseTree
    |> design
    |> absolutePositionTree
    |> (fun tree -> nodeToPoint tree @ treeLines tree)
    |> Chart.combine
    |> Chart.withXAxis mirroredXAxis
    |> Chart.withYAxis mirroredYAxis
    |> Chart.saveHtml (
        path = getSavePath(), 
        OpenInBrowser = true
    )
    0 // Return successful status code
