"""Advent of Code 2021 - Day 7."""

using Test


"""Return part 1 fuel need to move crabs from their initial positions to the target position."""
function fuel1(positions::AbstractVector{Int}, target::Int)::Int
    sum(abs(x - target) for x in positions)
end

test_data = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
@test [fuel1(test_data, i) for i in [2, 1, 3, 10]] == [37, 41, 39, 71]

"""Return part 2 fuel need to move crabs from their initial positions to the target position."""
function fuel2(positions::AbstractVector{Int}, target::Int)::Int
    sum_to(n) = n * (n + 1) ÷ 2
    sum(sum_to(abs(x - target)) for x in positions)
end

@test [fuel2(test_data, i) for i in [5, 2]] == [168, 206]

"""Find the minimum value of f(x) within the given bounds x_min <= x <= x_max."""
function find_minimum(f::Function, x_min::Int, x_max::Int)::Int
    while true
        if x_min == x_max
            return f(x_min)
        end
        x = (x_min + x_max) ÷ 2
        z = f(x)
        if x > x_min && f(x - 1) < z
            x_max = x - 1
        elseif x < x_max && f(x + 1) < z
            x_min = x + 1
        elseif f(x - 1) > z && f(x + 1) > z
            return z
        else
            error("confused in find_minimum at", x)
        end
    end
end

@test find_minimum(x -> fuel1(test_data, x), 0, 16) == 37
@test find_minimum(x -> fuel2(test_data, x), 0, 16) == 168

crabs = [parse(Int, s) for s in split(read("../aoc-data/input/2021_07.txt", String), ",")]
x_min, x_max = extrema(crabs)
println(find_minimum(x -> fuel1(crabs, x), x_min, x_max))
println(find_minimum(x -> fuel2(crabs, x), x_min, x_max))
