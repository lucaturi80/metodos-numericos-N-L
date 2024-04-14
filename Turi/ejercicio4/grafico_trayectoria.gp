set title "posicion del objeto en caida libre en funcion del tiempo"

set xrange[0:5]
set yrange[0:10]

set ylabel "posicion en metros"
set xlabel "tiempo en segundos"

set terminal png size 1024,768
set output "posicion_objeto.png"

plot 10 - 6.57781208 * x + 4.4142155758 - 4.4142155758 * exp(-1.49 * x)
