set logscale y

set title "comparacion del error relativo en x de los distintos metodos"

set xlabel "n° de iteraciones"
set ylabel "error en x"

set grid

set terminal pdf
set output "p1d.pdf"

plot "../resultados/p1_biseccion.dat" u 1:4 t "biseccion", "../resultados/p1_secante.dat" u 1:4 t "secante", "../resultados/p1_newton.dat" u 1:4 t "newton" 
