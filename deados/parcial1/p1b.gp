set xlabel "x"
set ylabel "y"

set xra [0:2]
set yra [-1.5:1.5]

set terminal pdf
set output "p1b.pdf"

plot cos(x ** 2 - 0.5) t "f(x)"
