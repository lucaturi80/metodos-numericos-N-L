set title "aproximacion ecuacion diferencial h=0.01" tc lt 2
set ylabel "y(t)"
set xlabel "t"

set xra [0:1]
e = 2.71828182846
pi = 3.14159265359
y(x) = (1 + (2*pi)/(1+4*pi**2)) * e **(-x) + (sin(2*pi*x)-2*pi*cos(2*pi*x))/(1+4*pi**2)

set title "error en aproximacion ecuacion diferencial h=0.01" tc lt 2
unset logscale y
set ylabel "y(t)"
set xlabel "t"
plot y(x) t "funcion exacta y", \
"data/euler01.dat" u 1:2 with linespoints linestyle 1 lc rgb "#98db98 " t "aproximacion con euler",\
"data/rk201.dat" u 1:2 with linespoints linestyle 2 lc rgb "#98dbd6" t "aproximacion con rk2",\
"data/rk401.dat" u 1:2 with linespoints linestyle 3 lc rgb "#989ddb" t "aproximacion con rk4"

pause mouse keypress

set title "error en aproximacion ecuacion diferencial h=0.005" tc lt 2
unset logscale y
set ylabel "y(t)"
set xlabel "t"
plot y(x) t "funcion exacta y", \
"data/euler005.dat" u 1:2 with linespoints linestyle 1 lc rgb "#98db98 " t "aproximacion con euler",\
"data/rk2005.dat" u 1:2 with linespoints linestyle 2 lc rgb "#98dbd6" t "aproximacion con rk2",\
"data/rk4005.dat" u 1:2 with linespoints linestyle 3 lc rgb "#989ddb" t "aproximacion con rk4"

pause mouse keypress
