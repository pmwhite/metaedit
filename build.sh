#!/usr/bin/env sh

set -eux

warnings="-w A-42-70-30 -error-style short"
ocamlopt $warnings me.ml -o me.exe
