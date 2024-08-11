#!/usr/bin/env sh

set -eux

warnings="-w A-40-42-70-30 -error-style short"
ocamlopt -c stubs.c
ocamlopt -o me.exe $warnings -I +unix unix.cmxa me.ml stubs.o
