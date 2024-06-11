set title "eq diferencial en los casos" tc lt 2
set ylabel "y(t)"
set xlabel "t"

set xra [0:10]
plot "data/eq_diff.dat" u 1:2 with linespoints linestyle 1 lc rgb "#98dbd6" t "caso w0 = 0.5",\
     "data/eq_diff.dat" u 1:3 with linespoints linestyle 2 lc rgb "#98bedb" t "caso w0 = 2.0",\
     "data/eq_diff.dat" u 1:4 with linespoints linestyle 3 lc rgb "#b598db" t "caso w0 = pi",\
     "data/eq_diff.dat" u 1:5 with linespoints linestyle 4 lc rgb "#d698db" t "caso w0 = 3.6",\
     "data/eq_diff.dat" u 1:6 with linespoints linestyle 5 lc rgb "#db98be" t "caso w0 = 5.5",\
     "data/eq_diff.dat" u 1:7 with linespoints linestyle 6 lc rgb "#989ddb" t "caso w0 = 2pi"
pause -1