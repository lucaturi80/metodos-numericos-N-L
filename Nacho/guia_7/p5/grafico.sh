set xra [0:200]

plot "data/datos.dat" u 1:2 with linespoints linestyle 1 t "S",\
     "data/datos.dat" u 1:3 with linespoints linestyle 2 t "I",\
     "data/datos.dat" u 1:4 with linespoints linestyle 3 t "R"

pause mouse keypress
