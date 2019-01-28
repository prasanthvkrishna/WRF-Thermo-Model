#!/bin/bash
make clean
cp openmp/thermo_2d.f90 src/thermo.f90
make
exit
