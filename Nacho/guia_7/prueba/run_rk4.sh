gfortran ../../precision.f90 ../../univ_const.f90 funciones.f90 ../Eq_Diff.f90 prg_rk4.f90 -o prg.x -J modulos_mod
./prg.x
gnuplot grafico.sh
rm prg.x