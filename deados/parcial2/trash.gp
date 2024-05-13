prfpot = 125.738
sigma = 0.334454
F(x) = 4 * prfpot * ( (sigma**12)*((-12)*x**(-13)) - (sigma**6)*((-6)* x**(-7)))
set xra [0.36:0.75]
#set yra[F(0.36)-0.000001 : F(0.36)+0.000001]
plot F(x),F(0.36)
pause -1