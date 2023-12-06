// -*- javascript -*-
// script-01.mjs: Solve Advent of Code 2023 2.1
// Copyright 2023 Timothy Sample <samplet@ngyro.com>
// SPDX-License-Identifier: CC0-1.0

import * as readline from "node:readline";
import * as process from "node:process";

function parseHandful(handfulStr) {
  // Split out each number-color pair (using ","), and trim each one
  // to remove extra whitespace.
  const colorCounts = handfulStr.split(",").map(x => x.trim());

  // Initialize the result.
  let handful = { red: 0, green: 0, blue: 0 };

  // Add each color count to the result.
  colorCounts.forEach(colorCount => {
    const [count, color] = colorCount.split(" ");
    handful[color] = parseInt(count);
  });

  return handful;
}

function parseGame(gameStr) {
  // Split the string on ":".
  const [idStr, handfulsStr] = gameStr.split(":");

  // Get the ID (by skipping five characters: "Game ").
  const id = parseInt(idStr.substring(5));

  // Split out the handfuls (on ";") and parse each one.
  const handfuls = handfulsStr.split(";").map(parseHandful);

  return { id, handfuls };
}

function isHandfulPossible(handful) {
  const { red, green, blue } = handful;
  return red <= 12 && green <= 13 && blue <= 14;
}

function isGamePossible(game) {
  return game.handfuls.every(isHandfulPossible);
}

// Initialize the result.
let sum = 0;

// Read lines from standard input.
const rl = readline.createInterface ({ input: process.stdin });
for await (const line of rl) {
  const game = parseGame(line);
  if (isGamePossible(game)) {
    sum += game.id;
  }
}

// Print the result.
console.log(sum);
