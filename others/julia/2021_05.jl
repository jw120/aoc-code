"""Advent of Code 2021 - Day 5."""

using Test

struct Point
    x::Int
    y::Int
end

struct Segment
    p1::Point
    p2::Point
end

function read_segment(s::AbstractString)::Segment
    m = match(r"(\d+),(\d+) -> (\d+),(\d+)", s)
    Segment(
        Point(parse(Int, m.captures[1]), parse(Int, m.captures[2])),
        Point(parse(Int, m.captures[3]), parse(Int, m.captures[4])))
end

"""Return the points associated with a segment"""
function segment_line(s::Segment, with_diagonals::Bool)::Vector{Point}
    if s.p1.y == s.p2.y
        x_min, x_max = extrema([s.p1.x, s.p2.x])
        [Point(x, s.p1.y) for x in range(x_min, x_max)]
    elseif s.p1.x == s.p2.x
        y_min, y_max = extrema([s.p1.y, s.p2.y])
        [Point(s.p1.x, y) for y in range(y_min, y_max)]
    elseif with_diagonals && abs(s.p1.x - s.p2.x) == abs(s.p1.y - s.p2.y)
        [Point(
            s.p1.x + i * sign(s.p2.x - s.p1.x),
            s.p1.y + i * sign(s.p2.y - s.p1.y))
         for i in range(0, abs(s.p1.x - s.p2.x))]
    else
        []
    end
end

function multiples(segments::AbstractVector{Segment}, with_diagonals::Bool)::Int
    counts = Dict()
    for s in segments
        for p in segment_line(s, with_diagonals)
            counts[p] = get(counts, p, 0) + 1
        end
    end
    sum(v > 1 for v in values(counts))
end

test_data = [
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2",
]

@test multiples(read_segment.(test_data), false) == 5
@test multiples(read_segment.(test_data), true) == 12

segments = [read_segment(d) for d in readlines("../aoc-data/input/2021_05.txt")]
println(multiples(segments, false))
println(multiples(segments, true))


