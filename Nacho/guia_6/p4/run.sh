gfortran ../../precision.f90 ../../univ_const.f90 funciones.f90 ../integrales_practico.f90  p4.f90 -o prg.x -J modulos_mod
./prg.x
rm prg.x
gnuplot graficoa.sh
gnuplot graficob.sh