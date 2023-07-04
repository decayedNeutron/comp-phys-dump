#!/usr/bin/gnuplot -persist

plot '20K_ThermoKBT1.dat' u 1:(column(3)+column(2)) w lines, '20K_ThermoKBT1.dat' u 1:2 w lines, '20K_ThermoKBT1.dat' using 1:3 w lines




