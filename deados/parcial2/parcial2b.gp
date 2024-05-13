set dummy x

F(x) = 4 * prfpot * ( (sigma / x)**12 - (sigma / x)**6 )   # Funcion  en x con parámetros a y b               

prfpot = 2  
sigma = 2

fit F(x) "data/datos.dat" via prfpot, sigma

set terminal pdf
set output "parcial2b.pdf"

set title "ajuste de V(r), y datos medidos" tc lt 2
set ylabel 'V(r)'
set xlabel 'r'
set grid

plot "data/datos.dat" w points pt 2 ps 0.5 title "Mediciones", F(x) title "Ajuste de v(r)"
pause -1

