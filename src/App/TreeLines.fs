module Lines 
    open Library.Jensen.TreeFit

    let fromTree t =
        let rec fromTree' depth pos acc (Node((_, c), ts)) =
            let acc' = (c + pos, -depth)::acc
            List.fold (fromTree' (depth + 1) (pos + c)) acc' ts
        fromTree' 0 0.0 [] t


