open Library.TreeDesign
open Lines

open Plotly.NET
open Plotly.NET.LayoutObjects


let t1 = Node("A", [])
let t2 = Node("B", [])

let t3 = Node("C", [ t1; t2 ])
let t4 = Node("D", [])

let t = Node("Root", [ t3; t4 ])


let dTree = design t
let absTree = absolutePositionTree dTree


let points = nodeToPoint absTree
let lines = treeLines absTree

let mirroredXAxis =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false)

let mirroredYAxis =
    LinearAxis.init (ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false)


points @ lines
|> Chart.combine
|> Chart.withXAxis mirroredXAxis
|> Chart.withYAxis mirroredYAxis
|> Chart.show
