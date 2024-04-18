program problema1

use isoprecisiones
use funciones
use metodos


implicit none

!defino las variables del programa

  real(kind=dp)        :: a 
  real(kind=dp)        :: b
  real(kind=dp)        :: tol_x
  real(kind=dp)        :: tol_y
  real(kind=dp)        :: x0
  real(kind=dp)        :: max_ite
  integer(kind=ed)     :: Nite
  integer(kind=ed)     :: u1=1_ed, u2=2_ed, u3=3_ed

!*************************************************************************
! f1: funcion a encontrar la raiz
! f2: derivada de la funcion
! a y b: limites inferiores y superiores del interbalo en x
! tol_x y tol_y: el error maximo permitido en x e y
! x0: raiz a encontrar
! max_ite: numero maximo de iteraciones permitidas
! Nite: numero final de iteraciones que le tomo al programa converger
! unidad: archivo de datos donde se van a inprimir los resulrtados
!************************************************************************

open(unit=u1,file="resultados/p1_biseccion.dat",status="replace",action="write")
open(unit=u2,file="resultados/p1_secante.dat",status="replace",action="write")
open(unit=u3,file="resultados/p1_newton.dat",status="replace",action="write")


call biseccion(f1,a,b,tol_x,tol_y,x0,max_ite,Nite,u1)
Nite = 0_ed

call secante(f1,a,b,tol_x,tol_y,x0,max_ite,Nite,u2)
Nite = 0_ed

call newton(f1,f2,a,tol_x,tol_y,x0,max_ite,Nite,u3)


close(u1)
close(u2)
close(u3)

end program problema1
