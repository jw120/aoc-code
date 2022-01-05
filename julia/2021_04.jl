"""Advent of Code 2021 - Day 4."""

struct BingoCard
    numbers::Matrix{Int}
    marked::Matrix{Bool}
end

"""Read a bingo card from a 5-line string"""
function read_bingo_card(s::AbstractString)::BingoCard
    words = [[parse(Int, w) for w in split(line)] for line in split(s, "\n")]
    numbers = reshape([words[i][j] for i in range(1, 5), j in range(1, 5)], 5, 5)
    marked = fill(false, 5, 5)
    BingoCard(numbers, marked)
end

"""Mark the bingo card for the give number, return true if card wins."""
function mark_card!(n::Int, b::BingoCard)::Bool
    position = first(indexin(n, b.numbers))
    if position === nothing
        return false
    end
    b.marked[position] = true

    for i in range(1, 5)
        if all(b.marked[i, :]) || all(b.marked[:, i])
            return true
        end
    end
    return false
end


"""Score of a board is sum of unmarked numbers."""
function score(b::BingoCard)::Int
    sum(b.numbers[i, j] * (1 - b.marked[i, j]) for i in range(1, 5), j in range(1, 5))
end

function first_win!(numbers::AbstractVector{Int}, bs::AbstractVector{BingoCard})::Int
    for n in numbers
        for b in bs
            if mark_card!(n, b)
                return score(b) * n
            end
        end
    end
    error("Failed!")
end

function last_win(numbers::AbstractVector{Int}, bs::AbstractVector{BingoCard})::Int
    indices = Set(range(1, length(bs)))
    for n in numbers
        for i in copy(indices)
            if mark_card!(n, bs[i])
                delete!(indices, i)
                if isempty(indices)
                    return score(bs[i]) * n
                end
            end
        end
    end
    error("Failed!")
end

blocks = split(read("../aoc-data/input/2021_04.txt", String), "\n\n")
numbers = [parse(Int, w) for w in split(blocks[begin], ",")]
cards = read_bingo_card.(blocks[begin+1:end])

println(first_win!(numbers, cards))
println(last_win(numbers, cards))
