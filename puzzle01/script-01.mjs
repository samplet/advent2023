// -*- javascript -*-
// script-01.mjs: Solve Advent of Code 2023 1.1
// Copyright 2023 Timothy Sample <samplet@ngyro.com>
// SPDX-License-Identifier: CC0-1.0

import * as readline from "node:readline";
import * as process from "node:process";

const rl = readline.createInterface ({ input: process.stdin });

// Initialize the result.
let sum = 0;

// Read lines from standard input.
for await (const line of rl) {
  // Remove nondigits.
  const digits = line.replace(/[^0-9]/g, "");

  // Get the first and last digit.
  const first = digits[0];
  const last = digits[digits.length - 1];

  // Get the numerical value.
  const value = parseInt(first + last);

  // Add it to our sum.
  sum += value;
}

// Print the result.
console.log(sum);
