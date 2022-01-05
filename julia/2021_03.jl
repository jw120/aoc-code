"""Advent of Code 2021 - Day 3."""

using Test

"""Convert a vector of strings of '1's and '0's to a matrix with the bit-strings as columns"""
function bit_strings_to_matrix(xs::Vector{String})::Matrix{Bool}
    reshape([c == '1' for s in xs for c in s], (length(xs[begin]), length(xs)))
end

"""Return most common element in a Vector{Bool}"""
function most_common(b::AbstractVector{Bool})
    sum(b) * 2 >= length(b)
end

"""Convert a bitvector to an int"""
function vb_to_int(x::AbstractVector{Bool})::Int64
    xbitmap = Int64(2) .^ collect(length(x)-1:-1:0)
    sum(xbitmap[x])
end

function part1(m::Matrix{Bool})
    mcb = most_common.(eachrow(m))
    gamma = vb_to_int(mcb)
    epsilon = vb_to_int(@. !mcb)
    gamma * epsilon
end

function downselect(m::Matrix{Bool}, follow_most_common::Bool)
    rows, cols = size(m)
    remaining = Set(range(1, cols))
    for position in range(1, rows)
        if length(remaining) <= 1
            break
        end
        mcb = most_common([m[position, c] for c in remaining])
        remaining = filter(i -> (m[position, i] == mcb) == follow_most_common, remaining)
    end
    vb_to_int(m[:, pop!(remaining)])
end

function part2(m::Matrix{Bool})
    downselect(m, true) * downselect(m, false)
end


test_data = bit_strings_to_matrix(["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"])

@test part1(test_data) == 198
@test part2(test_data) == 230

number_matrix = bit_strings_to_matrix(readlines("../aoc-data/input/2021_03.txt"))
println(part1(number_matrix))
println(part2(number_matrix))
