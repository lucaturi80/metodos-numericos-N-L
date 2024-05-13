prfpot = 125.738
sigma = 0.334454
F(x) = 4 * prfpot * ( (sigma**12)*(-12)*(-13)*(-14)* x**(-15) - (sigma**6)*(-6)*(-7)*(-8)* x**(-9))
set xra [0.36:0.75]
plot F(x),F(0.36)
pause -1