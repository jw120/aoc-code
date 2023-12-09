"""Advent of Code 2021 - Day 6."""

using Test

function fish(initial_timers::AbstractVector{Int}, number_of_steps::Int)::Int
    f = Dict((i, 1) for i in range(-8, 0))
    for i in range(1, number_of_steps)
        f[i] = f[i-1-8] + f[i-1-6]
    end
    sum(f[number_of_steps-x] for x in initial_timers)
end

@test [fish([3, 4, 3, 1, 2], i) for i in [1, 2, 18, 256]] == [5, 6, 26, 26984457539]

initial = [parse(Int, s) for s in split(read("../aoc-data/input/2021_06.txt", String), ",")]
println(fish(initial, 80))
println(fish(initial, 256))
