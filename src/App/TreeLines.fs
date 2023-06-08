module Lines

open Library.TreeDesign
open Plotly.NET



let nodePositions t =
    let rec fromTree' depth pos acc (Node((_, c), ts)) =
        let acc' = (c + pos, -depth) :: acc
        List.fold (fromTree' (depth + 1) (pos + c)) acc' ts

    fromTree' 0 0.0 [] t


let nodeToPoint t =
    let rec nodeToPoint' acc (Node((l, p), ts)) =
        let point =
            Chart.Point(
                [ p ],
                MultiText = [ l ],
                MultiTextPosition = [ StyleParam.TextPosition.TopCenter ],
                ShowLegend = false
            )

        point :: List.fold nodeToPoint' acc ts

    nodeToPoint' [] t


let treeLines t =
    let rec treeLines' acc ((Node((_, p), ts)) as t) =
        let makeLine (Node((_, p), _)) (Node((_, c), _)) =
            Chart.Line(xy = [ p; c ], LineColor = Color.fromString "black", ShowLegend = false)

        let linesToChildren = ts |> List.map (makeLine t)
        let childLines = List.fold treeLines' acc ts
        linesToChildren @ childLines //? TODO: Use function accumulator if optimization is needed

    treeLines' [] t
