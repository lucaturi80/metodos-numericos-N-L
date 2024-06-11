set xra [0:10]
#e = 2.71828
# y(x) = (x+1)**2 -0.5 * e**x 
plot "data/datos.dat" u 1:2 with linespoints linestyle 1
pause -1