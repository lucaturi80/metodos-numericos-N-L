set logscale y
set xlabel "x"
set ylabel "y"
set terminal pdf
set output "p1d.pdf"
plot "../resultados/p1_biseccion.dat" u 1:4 t "biseccion", "../resultados/p1_secante.dat" u 1:4 t "secante", "../resultados/p1_newton.dat" u 1:4 t "newton" 