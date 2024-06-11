
set title "FUNCION INCISO b" tc lt 2
set ylabel "aprox de integral"
set xlabel "n"
set xrange [2:100]
filedata =  "data/bintegral.dat"
plot filedata u 1:2 t "punto medio" , filedata u 1:3 t "trapecio",  filedata u 1:4 t "simpson", 0.1922593577
pause mouse keypress
filedata =  "data/berror.dat"
set ylabel "error de aproximacion"
set logscale y
plot filedata u 1:2 t "punto medio" , filedata u 1:3 t "trapecio", filedata u 1:4 t "simpson"
pause mouse keypress

