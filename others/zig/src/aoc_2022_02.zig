/// Advent of Code 2022, Day 02
const std = @import("std");
const rw = @import("rw");

// Errors from our input parsing code
const ParseError = error{ InvalidChar, WrongLength };

// Game outcome from player's point of view
const Outcome = enum(i64) {
    win = 6,
    lose = 0,
    draw = 3,
};

// Shapes used by the two players
const Shape = enum(i64) {
    rock = 1,
    paper = 2,
    scissors = 3,
    fn parse(c: u8) ParseError!Shape {
        return switch (c) {
            'A' => Shape.rock,
            'B' => Shape.paper,
            'C' => Shape.scissors,
            else => ParseError.InvalidChar,
        };
    }
    // Give outcome for self with given other player's shape
    fn resolve(self: Shape, other: Shape) Outcome {
        return switch (self) {
            Shape.rock => switch (other) {
                Shape.rock => Outcome.draw,
                Shape.paper => Outcome.lose,
                Shape.scissors => Outcome.win,
            },
            Shape.paper => switch (other) {
                Shape.rock => Outcome.win,
                Shape.paper => Outcome.draw,
                Shape.scissors => Outcome.lose,
            },
            Shape.scissors => switch (other) {
                Shape.rock => Outcome.lose,
                Shape.paper => Outcome.win,
                Shape.scissors => Outcome.draw,
            },
        };
    }
    // For iteration over possible shapes
    fn next(self: Shape) Shape {
        return switch (self) {
            .rock => .paper,
            .paper => .scissors,
            .scissors => .rock,
        };
    }
};

// Coding of response-part of problem inputs
const Response = enum(i64) {
    x,
    y,
    z,
    fn parse(c: u8) ParseError!Response {
        return switch (c) {
            'X' => Response.x,
            'Y' => Response.y,
            'Z' => Response.z,
            else => ParseError.InvalidChar,
        };
    }
    // For part (a) - interpret response as a shape
    fn to_shape(self: Response) Shape {
        return switch (self) {
            .x => Shape.rock,
            .y => Shape.paper,
            .z => Shape.scissors,
        };
    }
    // For part (b) - interpret response as an outcome
    fn to_outcome(self: Response) Outcome {
        return switch (self) {
            .x => Outcome.lose,
            .y => Outcome.draw,
            .z => Outcome.win,
        };
    }
};

// Problem inputs
const Game = struct {
    opponent: Shape,
    response: Response,
    fn parse(s: []const u8) ParseError!Game {
        if (s.len != 3) {
            return ParseError.WrongLength;
        }
        const opponent = try Shape.parse(s[0]);
        const response = try Response.parse(s[2]);
        return Game{
            .opponent = opponent,
            .response = response,
        };
    }
};

// Part (a) solution
fn part_a(games: []Game) i64 {
    var total: i64 = 0;
    for (games) |game| {
        const player = game.response.to_shape();
        total += @intFromEnum(player.resolve(game.opponent)) + @intFromEnum(player);
    }
    return total;
}

// Part (b) solution
fn part_b(games: []Game) i64 {
    var total: i64 = 0;
    for (games) |game| {
        const outcome = game.response.to_outcome();
        var player = Shape.rock;
        while (player.resolve(game.opponent) != outcome) {
            player = player.next();
        }
        total += @intFromEnum(outcome) + @intFromEnum(player);
    }
    return total;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    // Create list of their sums
    const games: []Game = try rw.read_parse_lines(Game, allocator, Game.parse);
    defer allocator.free(games);

    try rw.write_int(i64, part_a(games));
    try rw.write_int(i64, part_b(games));
}
