set title "grafico de cos(x²-0.5)"

set xlabel "x"
set ylabel "f(x)"

set xtics 0.1

set grid

set xra [0:2]
set yra [-1.5:1.5]

set terminal pdf
set output "p1b.pdf"

plot cos(x ** 2 - 0.5) t "cos(x²-0.5)"
