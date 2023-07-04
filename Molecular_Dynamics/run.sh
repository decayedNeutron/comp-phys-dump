#!/bin/bash






gfortran MDkbT1.f90 -o tex.x
gfortran MDkbT2.f90 -o texx.x

echo 'Running for KBT = 1'
./tex.x
echo 'Running for kBT = 2'
./texx.x

