set title "derivada de V(r), analiticamente y con formula de 3 puntos centrada" tc lt 2
prfpot = 125.738
sigma = 0.334454
F(x) = 4 * prfpot * ( (sigma**12)*((-12)*x**(-13)) - (sigma**6)*((-6)* x**(-7)))

set ylabel "dV(r)"
set xlabel "r"
set xrange [0.36:0.75]
set terminal pdf
set output "parcial2c.pdf"
plot F(x) t "estimacion ajustada", "data/parcial2c.dat" u 1:2 t "estimacion con f3c"


set terminal pdf
set output "parcial2c_error.pdf"
set ylabel "error en escala logaritmica"
set logscale y
set yrange [0.01:376]
plot "data/parcial2c.dat" u 1:3 t "error absoluto"
