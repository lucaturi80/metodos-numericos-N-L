set xra [0:10]
set yra [-0.5:0.5]
#e = 2.71828
# y(x) = (x+1)**2 -0.5 * e**x 
plot "data/datosa.dat" u 1:2 with linespoints linestyle 1
pause mouse keypress
plot "data/datosb.dat" u 1:2 with linespoints linestyle 1
pause mouse keypress
