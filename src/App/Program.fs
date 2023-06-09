// #############################################
// # Authors: Alina & Mads                     #
// # Date: June 6th                            #
// # Last edit: June 6th                       #
// #############################################

open System.IO
open Library.TreeDesign

open Plotly.NET
open Plotly.NET.LayoutObjects

open Lines

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
    | Some s -> Node(s, []) //TODO: Insert actual implementation here if necessary
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
