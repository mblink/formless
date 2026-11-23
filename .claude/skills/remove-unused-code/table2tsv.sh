#!/usr/bin/env bash
# Converts find-unused's rendered output table(s) into "name<TAB>location" lines
# Usage: table2tsv.sh all.txt > all.tsv
awk -F'│' '
  /^│/ && NF == 4 {
    for (i = 2; i <= 3; i++) gsub(/^[ \t]+|[ \t]+$/, "", $i)
    if ($2 != "Instance") print $2 "\t" $3
  }
' "$1"
