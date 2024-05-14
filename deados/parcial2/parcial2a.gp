set title "energia pontencial de Lennard-Jones (V(r)) en funcion de la distancia (r) entre los atomos"

set terminal pdf 
set output "parcial2a.pdf"

set xlabel "r (en nm) distancia entre los atomos"
set ylabel "V(r) (en erg) potencial de Lennard-Jones"

set grid

set key box right

plot "data/datos.dat" u 1:2 t 'V(r)'
