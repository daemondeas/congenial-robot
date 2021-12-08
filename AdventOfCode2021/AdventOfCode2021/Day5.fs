module Day5

open System.Collections.Generic

    type Point =
        { X : int
          Y : int }

    type Line =
        { Start : Point
          End   : Point }

    let isHorizontal line =
        line.Start.X = line.End.X

    let isVertical line =
        line.Start.Y = line.End.Y

    let isHorizontalOrVertical line =
        (isHorizontal line) || (isVertical line)

    let countPoint (points: Dictionary<Point, int>) point =
        if points.ContainsKey point then
            points.[point] <- points.[point] + 1
        else
            points.[point] <- 1
        points
