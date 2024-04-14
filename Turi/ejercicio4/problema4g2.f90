program P4G2

use isoprecisiones
use funciones
use modulos

implicit none

real(kind=rd) :: a = 2.0_rd
real(kind=rd) :: b = 2.5_rd
real(kind=rd) :: tol_x = 0.0001_rd
real(kind=rd) :: tol_y = 0.0001_rd
real(kind=rd) :: max_ite = 5000
real(kind=rd) :: raizn, raizb
integer(kind=id) :: Nite
real(kind=rd) :: x
integer(kind=is) :: u1 = 10, u2 = 20

open(unit=u1,file="datos_newton.dat",status="replace",action="write")
open(unit=u2,file="datos_bisec.dat",status="replace",action="write")

call newton(mov,dmov,b,tol_x,tol_y,raizn,max_ite,Nite,u1)
call biseccion(mov,a,b,tol_x,tol_y,raizb,max_ite,Nite,u2)

call system('gnuplot -persist grafico_trayectoria.gp')

print*, "tiempo que tarda en llegar al suelo"
print*,"newton","                                ","bisecion"
print*,raizn,"        ",raizb

close(u1)
close(u2)

end program P4G2

