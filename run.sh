#!/bin/bash

stack exec compiler "$1".in > "$1".ll
llvm-as "$1".ll -o "$1".bc
lli "$1".bc
