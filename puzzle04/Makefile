# Makefile: Build program for Advent of Code 4
# Copyright 2023 Timothy Sample <samplet@ngyro.com>
# SPDX-License-Identifier: CC0-1.0

part1: part1.tab.c part1.yy.c
	gcc -g -lfl -o "$@" $^

part1.tab.c: part1.y
	bison -d -o "$@" "$<"

part1.yy.c: part1.l
	flex -o "$@" "$<"
