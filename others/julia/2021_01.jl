"""Advent of Code 2021 - Day 1."""

using Test

"Number of values that are higher than the preciding value"
function increases(v)
    sum(x < y for (x, y) in zip(v, v[begin+1:end]))
end

@test increases([1, 2, 4, 3, 5]) == 3

"Apply three-value sliding window sum"
function sliding(v)
    [x + y + z for (x, y, z) in zip(v, v[begin+1:end], v[begin+2:end])]
end

@test sliding([1, 2, 3, 4, 5]) == [6, 9, 12]

depths = [parse(Int, d) for d in readlines("../aoc-data/input/2021_01.txt")]
println(increases(depths))
println(increases(sliding(depths)))
