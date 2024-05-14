prfpot = 125.738
sigma = 0.334454
#v3ra(x) = 4 * prfpot * ( (sigma**12)*(-12)*(-13)*(-14)*x**(-15) - (sigma**6)*(-6)*(-7)*(-8) * x**(-7))
#set xra [0.36:0.75]
#set yra[F(0.37)-0.000001 : F(0.37)+0.000001]
#plot F(x),F(0.37)
#pause -1


V(x) = 4 * prfpot * ( (sigma/x)**12 - (sigma/x)**6)

set xrange [0.37:0.40]
set yrange [-175:200]

plot V(x), 0
pause -1