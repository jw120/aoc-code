"""Advent of Code 2021 - Day 2."""

using Test

"""Return distance and depth changes for a given command."""
function change(s)
    command, argument_str = split(s)
    argument = parse(Int, argument_str)
    if command == "forward"
        (argument, 0)
    elseif command == "down"
        (0, argument)
    elseif command == "up"
        (0, -argument)
    else
        error("Unknown command")
    end
end

function run1(commands)
    horiz, depth = reduce(((a, b), (c, d)) -> (a + c, b + d), commands, init = (0, 0))
    horiz * depth
end

@test run1(map(change, ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])) == 150

function run2(commands)
    update((horiz, depth, aim), (h, d)) = (horiz + h, depth + h * aim, aim + d)
    horiz, depth, _ = foldl(update, commands, init = (0, 0, 0))
    horiz * depth
end

@test run2(map(change, ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"])) == 900

commands = map(change, readlines("../aoc-data/input/2021_02.txt"))
println(run1(commands))
println(run2(commands))
