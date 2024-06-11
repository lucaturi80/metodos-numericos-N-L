program dater
!-----------------------------------------
!precision
!-----------------------------------------
use isoprec
!INTEGER(KIND=il),parameter ::ni=il, nr=rd
!-----------------------------------------
use funciones

!-----------------------------------------
!declaracion de variables
!-----------------------------------------
INTEGER(KIND=ni)            ::i,n
REAL(KIND=nr)               ::x,h,a,b


open(unit=10, file="data.dat")
n=20
a=0._nr
b=3.14_nr
h=(b-a)/n
!-----------------------------------------

!-----------------------------------------
!proceso
!-----------------------------------------
x=a
do while(x<=b)
    write(10,*) x,f(x)
    x=x+h
end do

!-----------------------------------------

end program