

filen = "20K1200_ThSingel.dat"

while (1) {

    plot filen using 1:2 with lines, filen using 1:3 with lines, filen using 1:($2+$3) with lines
    pause 1    # waiting time in seconds
}
