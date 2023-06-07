module TreeDistanceTest

open NUnit.Framework
open Library.TreeDesign
open FsCheck.NUnit
open TGenerators

// • Two nodes at the same level are placed a given distance apart. => minimum_distance_check
// 1. non-overlapping for extends
// 2. all distance >= minimum distance