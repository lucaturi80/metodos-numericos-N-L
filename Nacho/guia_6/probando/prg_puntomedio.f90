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

INTEGER(KIND=pni)                        :: n = 18 
INTEGER(KIND=pni)                        :: i
REAL(KIND=pnr)                           ::a=0._pnr,b=3.14_pnr, h,trash, integral1, er1, integral2, er2, Q
REAL(KIND=pnr), allocatable, dimension(:)           ::x, fx, x1, fx1
allocate(x(-1:n+1))
allocate(fx(-1:n+1))

open(unit=10, file="data/data.dat")
!------------------------------------

!------------------------------------
!leyendo fx
!------------------------------------
do i=-1,n+1
read(10,*) x(i), fx(i)
end do
!------------------------------------

!------------------------------------
!proceso  para n = 18 (h = (b-a)/20 ) 
!------------------------------------
n= 18
h=(b-a) / (n+2)
call punto_medio(n,h,fx,integral2)
write(*,*) "la integral es: ", integral2
!------------------------------------


!
!!------------------------------------
!!reajustando para cambiar n por 8
!!------------------------------------
!n= 8
!allocate(x1(-1:n+1))
!allocate(fx1(-1:n+1))
!do i = 0, 10
!x1(i-1) = x(2*i-1)
!fx1(i-1) = fx(2*i-1)
!end do
!!------------------------------------
!
!
!!------------------------------------
!!proceso  para n = 8 (h = (b-a)/10 )
!!------------------------------------
!call punto_medio(n,a,b,fx1,integral1)
!write(*,*) "la integral es: ", integral1
!!------------------------------------
!
!
!!------------------------------------
!!validacion de calculo
!!------------------------------------
!er1 = abs(integral1 - (prim_f(b) - prim_f(a)))
!er2 = abs(integral2 - (prim_f(b) - prim_f(a)))
!
!Q = er1 / er2 
!write(*,*) "el cociente es ", Q
!!------------------------------------

end program