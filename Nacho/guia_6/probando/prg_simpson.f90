program prg

use integrales
use funciones
use isoprec
implicit none
!-----------------------------------------
!precision
!-----------------------------------------
INTEGER(KIND=il),parameter ::pni=il, pnr=rd
!-----------------------------------------

!------------------------------------
!declaracion de variables
!------------------------------------

INTEGER(KIND=pni)                        :: n = 20 
INTEGER(KIND=pni)                        :: i
REAL(KIND=pnr)                           ::a=0._pnr,b=3.14_pnr, trash, integral1, er1, integral2, er2, Q
REAL(KIND=pnr), allocatable, dimension(:)           ::x, fx, x1, fx1
allocate(x(0:n))
allocate(fx(0:n))

open(unit=10, file="data/data.dat")
!------------------------------------

!------------------------------------
!leyendo fx
!------------------------------------
do i=0,n
read(10,*) x(i), fx(i)
end do
!------------------------------------

!------------------------------------
!proceso para n = 20
!------------------------------------
call SIMPSON_equidis(n,a,b,fx,integral2)
!------------------------------------

!------------------------------------
!reajustando para cambiar n por 10
!------------------------------------
n= 10
allocate(x1(0:n))
allocate(fx1(0:n))
do i = 0, 10
x1(i) = x(2*i)
fx1(i) = fx(2*i)
end do
write(*,*) x1(:), fx1(:)
!------------------------------------


!------------------------------------
!proceso  para n = 10 (h = (b-a)/10 )
!------------------------------------
call SIMPSON_equidis(n,a,b,fx1,integral1)
write(*,*) "la integral es: ", integral1
!------------------------------------


!------------------------------------
!validacion de calculo
!------------------------------------
er1 = abs(integral1 - (prim_f(b) - prim_f(a)))
er2 = abs(integral2 - (prim_f(b) - prim_f(a)))
write(*,*) "er1 es", er1, "er2 es ", er2
Q = er1 / er2 
write(*,*) "el cociente es ", Q
!------------------------------------
end program