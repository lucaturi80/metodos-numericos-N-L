set dummy x

# Definimos la función a ajustar
F(x) = 4 * prfpot * ( (sigma / x)**12 - (sigma / x)**6 )   # Funcion  en x con parámetros a y b               

# Estimacion inicial de los parámetros
prfpot = 2  
sigma = 2

# Archivo de datos
fileData = "data/datos.dat"
fileGraf = "parcial2b.pdf"

# Realización del Ajuste
# Nuestro conjunto de datos están guardado en el archivo tipo texto llamado "decaimiento.txt"
fit F(x) fileData via prfpot, sigma


# Comienzo del Gráfico
##############################

# Definimos la terminal a utilizar y el nombre del grafico.
set terminal pdf
set output fileGraf

set title "Grafico del Ajuste Lineal   " tc lt 2
set label 'valores ajustados '
set grid

plot fileData w points pt 2 ps 0.5 title "Mediciones", F(x) title "Ajuste de v(r)"
pause -1

