module Tests.HelperFunc

open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Library.TreeDesign
open TGenerators

[<SetUp>]
let Setup () = ()

// fun mean test
let meanSymmetryProp (a,b) = mean(a,b)=mean(b,a)
[<Property(Arbitrary=[|typeof<NormalFloatGenerators>|])>]
let symmetryOfMeanTest (a, b) = meanSymmetryProp (a, b)
