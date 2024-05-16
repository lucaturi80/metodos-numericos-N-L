prfpot = 125.738
sigma = 0.334454
Vtercera(x) = 4 * prfpot * ( (sigma**12)*(-12)*(-13)*(-14)* x**(-15) - (sigma**6)*((-6)*(-7)*(-8)* x**(-9)))
set xra [0.37:0.74]

set terminal pdf
set output "parcial2c_erroryf3_1.pdf"
set title "funcion que aproxima al error"
plot "data/parcial2c.dat" u 1:3 t "error absoluto", abs(Vtercera(x)* 0.005**2 / 6) t "V'''*h**2/6"

set terminal pdf
set output "parcial2c_erroryf3_2.pdf"
set title "funcion que acota al error"
plot "data/parcial2c.dat" u 1:3 t "error absoluto", abs(Vtercera(x-0.005)* 0.005**2 / 6) t "V'''*h**2/6"
