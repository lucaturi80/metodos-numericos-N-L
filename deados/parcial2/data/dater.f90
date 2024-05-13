program dater

use isoprec
use funciones
INTEGER(KIND = il), PARAMETER   ::ni = il
!INTEGER(KIND = il), PARAMETER   ::nr = rd

REAL (KIND = rd)             :: x, a, b
INTEGER(KIND=ni)             :: i, n

a= 0
b= 1
n=7

open(unit=10, file="data/datos.dat")

write(10,*) n
do i= 1,n+1
    x=  a + ((b-a)/n ) * (i-1)
    write(10,66) x, f(x)
66 format (2(F18.9,x))
end do 
close(10)
end program