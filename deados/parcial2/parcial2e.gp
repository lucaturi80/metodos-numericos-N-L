set title 'garfico de las interpolaciones usando lagrange y del ajuste de gnuplot'

set terminal pdf
set output 'parcial2e.pdf'

prfpot= 125.7380
sigma = 0.334454

V(x) = 4 * prfpot * ( (sigma/x)**12 - (sigma/x)**6)

set xlabel "r (en nm) distancia entre los atomos"
set ylabel "V(r) (en erg) potencial de Lennard-Jones"

set key box right

set grid

set xrange [0.3:0.75]
set yrange [-175:200]

plot 'data/parcial2e.dat' with points t 'funcion interpolada con lagrange',V(x) t 'funcion ajustada con gnuplot'




