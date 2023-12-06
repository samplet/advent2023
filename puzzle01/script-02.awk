# script-02.awk: Solve Advent of Code 2023 1.2
# Copyright 2023 Timothy Sample <samplet@ngyro.com>
# SPDX-License-Identifier: CC0-1.0

function reverse(s, x) {
    x = "";
    for (i = 1; i <= length(s); i++) {
        x = (substr(s, i, 1) x);
    }
    return x;
}

BEGIN {
    sum = 0;

    map["0"] = "0";
    map["1"] = "1";
    map["2"] = "2";
    map["3"] = "3";
    map["4"] = "4";
    map["5"] = "5";
    map["6"] = "6";
    map["7"] = "7";
    map["8"] = "8";
    map["9"] = "9";

    map["zero"] = "0";
    map["one"] = "1";
    map["two"] = "2";
    map["three"] = "3";
    map["four"] = "4";
    map["five"] = "5";
    map["six"] = "6";
    map["seven"] = "7";
    map["eight"] = "8";
    map["nine"] = "9";

    map["orez"] = "0";
    map["eno"] = "1";
    map["owt"] = "2";
    map["eerht"] = "3";
    map["ruof"] = "4";
    map["evif"] = "5";
    map["xis"] = "6";
    map["neves"] = "7";
    map["thgie"] = "8";
    map["enin"] = "9";
}

{
    line = $0;
    enil = reverse(line);

    p = match(line, /(one|two|three|four|five|six|seven|eight|nine|[0-9])/, ms);
    d1 = map[ms[1]];
    p = match(enil, /(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9])/, ms);
    d2 = map[ms[1]];

    sum += (d1 d2);
}

END {
    print(sum);
}
