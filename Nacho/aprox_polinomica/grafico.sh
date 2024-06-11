
set autoscale

plot "data/funcion.dat"  u 1:2 t "aprox polinomica", "data/datos.dat" u 2:3 t "datos"
pause -1