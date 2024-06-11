set xra [0:200]

# Etiquetas de los ejes
set xlabel 't' font 'Verdana,12'
set ylabel 'Proporción'font 'Verdana,12'

# Paleta de colores
set style fill solid 1.0 border -1

# Cargar datos y apilar áreas
plot 'data/datos.dat' using 1:($2+$3+$4) with filledcurves x1 lt rgb "blue" title 'R', \
     'data/datos.dat' using 1:($2+$3) with filledcurves x1 lt rgb "green" title 'I', \
     'data/datos.dat' using 1:2 with filledcurves x1 lt rgb "yellow" title 'S'
pause -1