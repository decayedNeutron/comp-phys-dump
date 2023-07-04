#!/bin/bash

# Author : Paritosh


echo "This Maneuver Will Cost us 5 hrs. Plz GOD DO NOT CAUSE ERROR I MMA Sleep"

echo 'Compiling ising7.f90'
gfortran ising7.f90 -o i7.x

echo 'Compiling ising8.f90'
gfortran ising8.f90 -o i8.x

echo 'Compiling ising9.f90'
gfortran ising9.f90 -o i9.x

echo 'Running ISING for 7 x 7 x 7'
./i7.x

echo 'Running ISING for 8 x 8 x 8'
./i8.x

echo 'Running ISING for 9 x 9 x 9'
./i9.x

echo 'Swaaaaahaaaaaaa!!!!'
