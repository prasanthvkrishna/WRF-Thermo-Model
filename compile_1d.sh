#!/bin/bash
make clean
cp openmp/thermo_1d.f90 src/thermo.f90
make
exit
