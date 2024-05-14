prfpot = 125.738
sigma = 0.334454
Vtercera(x) = 4 * prfpot * ( (sigma**12)*(-12)*(-13)*(-14)* x**(-15) - (sigma**6)*((-6)*(-7)*(-8)* x**(-9)))
set xra [0.37:0.74]

set terminal pdf
set output "parcial2c_drvda3ra.pdf"
plot Vtercera(x)
pause -1