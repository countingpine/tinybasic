#!/bin/sh

echo "Building rtlib"

g++ -O2 -g -c rtlib/src/rtlib.cpp -o o/rtlib.o
g++ -O2 -g -c rtlib/src/startup.cpp -o o/startup.o
g++ -O2 -g -c rtlib/src/tb_asc.cpp -o o/tb_asc.o
g++ -O2 -g -c rtlib/src/tb_chr.cpp -o o/tb_chr.o
g++ -O2 -g -c rtlib/src/tb_lcase.cpp -o o/tb_lcase.o
g++ -O2 -g -c rtlib/src/tb_print.cpp -o o/tb_print.o
g++ -O2 -g -c rtlib/src/tbstring.cpp -o o/tbstring.o
g++ -O2 -g -c rtlib/src/tbstring-mm.cpp -o o/tbstring-mm.o
g++ -O2 -g -c rtlib/src/tb_ucase.cpp -o o/tb_ucase.o
g++ -O2 -g -c rtlib/src/tb_val.cpp -o o/tb_val.o

echo "Building compilers"

fbc -g -x tbc compiler/src/tinybasic.bas
./tbc -O -g -o tbc1 compiler/src/tinybasic.bas
./tbc1 -O -g -o tbc2 compiler/src/tinybasic.bas
./tbc2 -O -g -o tbc3 compiler/src/tinybasic.bas
./tbc3 -O -g -o tbc4 compiler/src/tinybasic.bas
