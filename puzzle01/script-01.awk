# script-01.awk: Solve Advent of Code 2023 1.1
# Copyright 2023 Timothy Sample <samplet@ngyro.com>
# SPDX-License-Identifier: CC0-1.0

BEGIN {
    sum = 0;
}

{
    line = $0;
    gsub(/[^0-9]/, "", line);
    sum += (substr(line, 1, 1) substr(line, length(line), 1));
}

END {
    print(sum);
}
