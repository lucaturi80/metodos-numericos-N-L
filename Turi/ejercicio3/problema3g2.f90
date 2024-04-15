program P3G2

use isoprecisiones
use funciones
use modulos

implicit none

real(kind=rd) :: a = 0.0_rd
real(kind=rd) :: b = 2.5_rd
real(kind=rd) :: tol_x = 0.0000000001_rd
real(kind=rd) :: tol_y = 0.0000000001_rd
real(kind=rd) :: max_ite = 5000
real(kind=rd) :: raiz
integer(kind=id) :: Nite
real(kind=rd) :: x
integer(kind=is) :: u1 = 10, u2 = 20

open(unit=u1,file="datos_newton.dat",status="replace",action="write")
open(unit=u2,file="datos_bisec.dat",status="replace",action="write")

call newton(f_b,df_b,b,tol_x,tol_y,raiz,max_ite,Nite,u1)
call biseccion(f_b,a,b,tol_x,tol_y,raiz,max_ite,Nite,u2)

call system('gnuplot -persist gnuplot_grafico.gp')

close(u1)
close(u2)

end program P3G2
