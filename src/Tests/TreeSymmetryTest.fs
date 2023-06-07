module Tests.TreeSymmetryTest


open NUnit.Framework
open Library.TreeDesign
open FsCheck
open FsCheck.NUnit


// Drawings must be symmetric wrt. reflection. => symmetryProperty
// relative tree
// reflect once

// TODO Check with no assumption of order
let rec checkRelativeReflection (Node((l, p: float), ts)) (Node((l', p'), ts')) =
    let notReflected t1 t2 = not <| checkRelativeReflection t1 t2

    l = l' 
    && p = -p' 
    && not <| List.exists2 (notReflected) ts ts' 

// TODO Check with no assumption of order
let rec checkAbsoluteReflection  (Node((l, (x, y)), ts)) (Node((l', (x', y')), ts')) =
    let notReflected t1 t2 = not <| checkAbsoluteReflection t1 t2

    l = l' 
    && x = -x' 
    && y = y 
    && not <| List.exists2 (notReflected) ts ts' 

let rec checkPositionalEquality (Node((l, p), ts)) (Node((l', p'), ts')) =
    let notEqual t1 t2 = not <| checkPositionalEquality t1 t2    

    l = l'
    && p = p'
    && not <| List.exists2 (notEqual) (List.sort ts) (List.sort ts')

[<Property>]
let ``reflect relative tree once to check is position negative`` (t:Tree<char>) = 
    let dt = design t
    let rt = reflectpos dt
    checkRelativeReflection dt rt 

// flect twice

[<Property>]
let ``reflect relative tree twice to check is position the same`` (t:Tree<char>) = 
    let dt = design t
    let rrt = reflectpos <| reflectpos dt
    dt = rrt

[<Property>]
let ``reflect absolute tree once to check is position negative`` (t:Tree<char>) = 
    let dt = absDesign t
    let rt = reflectAbs dt
    checkAbsoluteReflection dt rt 
// flect twice

[<Property>]
let ``reflect absolute tree twice to check is position the same`` (t:Tree<char>) = 
    let dt = absDesign t
    let rrt = reflectAbs <| reflectAbs dt
    dt = rrt

[<Property>]
let ``reflect used before or after design is equivalent`` (t: Tree<char>) =
    let dt1 = t |> reflect |> design
    let dt2 = t |> design |> reflectpos
    checkPositionalEquality dt1 dt2

[<Property>]
let ``reflect used before or after absDesign is equivalent`` (t: Tree<char>) =
    let dt1 = t |> reflect |> absDesign
    let dt2 = t |> absDesign |> reflectAbs
    checkPositionalEquality dt1 dt2