prfpot = 125.738
sigma = 0.334454
F(x) = 4 * prfpot * ( (sigma**12)*((-12)*x**(-13)) - (sigma**6)*((-6)* x**(-7)))

set xrange [0.36:0.75]
set terminal pdf
set output "parcial2c_derivada.pdf"
plot F(x), "data/parcial2c.dat" u 1:2


set terminal pdf
set output "parcial2c_error.pdf"
set logscale y
set yrange [0.01:376]
plot "data/parcial2c.dat" u 1:3
