set title "error relativo del metodo de newton en funcion de las iteraciones vs metodo de biseccion"

set autoscale

set xlabel "n° de iteraciones"
set ylabel "valor del error relativo en x"

set xzeroaxis
set yzeroaxis

set terminal png size 1024,768
set output "comparacion newton _ biseccion.png"

set logscale xy

plot "datos_newton.dat" u 1:3 t "error relativo newton" with points, "datos_bisec.dat" u 1:3 t "error relativo biseccion"
