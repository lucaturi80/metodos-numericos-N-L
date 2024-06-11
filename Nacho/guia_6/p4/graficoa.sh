
set title "FUNCION INCISO a" tc lt 2
set ylabel "aprox de integral"
set xlabel "n"
set xrange [2:100]
filedata =  "data/aintegral.dat"
plot filedata u 1:2 t "punto medio" , filedata u 1:3 t "trapecio",  filedata u 1:4 t "simpson", -0.2670627852
pause mouse keypress "apretar una letra"
filedata =  "data/aerror.dat"
set ylabel "error de aproximacion"
set logscale y
plot filedata u 1:2 t "punto medio" , filedata u 1:3 t "trapecio", filedata u 1:4 t "simpson",0
pause mouse keypress

