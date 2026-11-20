#!/bin/bash

# Checks conversions between source formats (`superbol convert`) against
# GnuCOBOL: for every COBOL source file found in the given directories (`test`
# by default) that compiles with `cobc` in FIXED or FREE format, and for each
# target format (with or without a right margin), checks that
#
# - the converted file still compiles;
# - `cobc -E` gives the same output for both files, up to whitespace, line
#   markers, and concatenations of alphanumeric literals (`"a" & "b"`).
#
# Debugging lines are compiled (`-fdebugging-line`), so that their contents
# are checked as well.
#
# Further, every program `*.cob` of `test/format_conversions` is compiled and
# run before and after conversion, and must give the same output.
#
# Environment variables: SUPERBOL (`superbol` executable to test), COBC
# (`cobc` executable), and TARGETS (space-separated target formats, each with
# an optional right margin, as in `FREE:72`).

set -uo pipefail

SUPERBOL="${SUPERBOL:-_build/default/src/lsp/superbol-free/main.exe}"
COBC="${COBC:-cobc}"
TARGETS="${TARGETS:-FIXED COBOL85 VARIABLE XCARD COBOLX FREE FREE:72 VARIABLE:80 COBOLX:72 XCARD:100}"
RUN_DIR="test/format_conversions"
SUPERBOL="$(realpath "$SUPERBOL")"

if [ $# -eq 0 ]; then set -- test; fi

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Normalized output of `cobc -E`, on its standard input
normalize () {
    grep -v '^#line' | sed 's/#area_a/ /g' | tr '\n\t\r' '   ' |
        sed -E "s/\"[[:space:]]*&[[:space:]]*\"//g;
                s/'[[:space:]]*&[[:space:]]*'//g;
                s/[[:space:]]+/ /g;
                s/[[:space:]]*([.,;()])[[:space:]]*/\1/g;
                s/^ //; s/ \$//"
}

# Debugging lines are compiled, so their contents are checked as well
COBC_FLAGS=(-fdebugging-line)

# cobc_ok FORMAT FILE INCDIR [OPTIONS...]
cobc_ok () {
    local fmt="$1" file="$2" incdir="$3"; shift 3
    # in a subshell, so crashes of cobc on some inputs are not reported
    ( "$COBC" "${COBC_FLAGS[@]}" -fformat="$fmt" -I "$incdir" "$@" "$file"
      exit $? ) >/dev/null 2>&1
}

# convert FORMAT TARGET FILE OUTPUT
convert () {
    local fmt="$1" target="${2%%:*}" margin="${2#*:}" file="$3" out="$4"
    local args=(convert --source-format "$fmt" --to-format "$target")
    if [ "$margin" != "$2" ]; then args+=(--right-margin "$margin"); fi
    "$SUPERBOL" "${args[@]}" "$file" > "$out" 2>/dev/null
}

lower () { tr '[:upper:]' '[:lower:]'; }

checked=0
failures=0

fail () {
    echo "FAILED: $*"
    failures=$((failures + 1))
}

# Comparison of preprocessed texts

n=0
while IFS= read -r -d '' file; do
    n=$((n + 1))
    dir="$WORK/$n"; mkdir -p "$dir"
    # short names, as cobc rejects long ones
    src="$dir/p.cob"; cp "$file" "$src"
    incdir="$(dirname "$file")"
    fmt=
    for f in fixed free; do
        if cobc_ok "$f" "$src" "$incdir" -fsyntax-only; then fmt=$f; break; fi
    done
    [ -n "$fmt" ] || continue
    checked=$((checked + 1))
    "$COBC" "${COBC_FLAGS[@]}" -E -fformat="$fmt" -I "$incdir" "$src" \
            2>/dev/null |
        normalize > "$dir/ref"
    for target in $TARGETS; do
        tfmt="$(echo "${target%%:*}" | lower)"
        out="$dir/c.cob"
        if ! convert "$(echo "$fmt" | lower)" "$target" "$src" "$out"; then
            fail "$file: conversion into $target"
        elif ! cobc_ok "$tfmt" "$out" "$incdir" -fsyntax-only; then
            fail "$file: conversion into $target does not compile"
        elif ! "$COBC" "${COBC_FLAGS[@]}" -E -fformat="$tfmt" -I "$incdir" \
                "$out" 2>/dev/null |
                normalize | cmp -s - "$dir/ref"; then
            fail "$file: conversion into $target is not equivalent"
        fi
    done
done < <(find "$@" -type f \( -iname '*.cob' -o -iname '*.cbl' \) -print0 |
             sort -z)

echo "Checked conversions of $checked files into: $TARGETS"

# Execution of programs

for prog in "$RUN_DIR"/*.cob; do
    [ -f "$prog" ] || continue
    dir="$WORK/run"; rm -rf "$dir"; mkdir -p "$dir"
    cp "$prog" "$dir/p.cob"
    if ! (cd "$dir" && "$COBC" "${COBC_FLAGS[@]}" -x -free -o p p.cob &&
              ./p > ref.txt); then
        fail "$prog: does not compile or run"
        continue
    fi
    for target in $TARGETS; do
        tfmt="$(echo "${target%%:*}" | lower)"
        if ! convert free "$target" "$dir/p.cob" "$dir/c.cob"; then
            fail "$prog: conversion into $target"
        elif ! (cd "$dir" && "$COBC" "${COBC_FLAGS[@]}" -x -fformat="$tfmt" \
                    -o c c.cob &&
                    ./c > out.txt && cmp -s ref.txt out.txt); then
            fail "$prog: conversion into $target does not give the same output"
        fi
    done
    echo "Checked execution of $prog"
done

if [ $failures -ne 0 ]; then
    echo "$failures failure(s)"
    exit 1
fi
