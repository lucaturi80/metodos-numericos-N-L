program cota_redondeo
use funciones

IMPLICIT NONE

REAL(KIND = nr)         ::x 
INTEGER(KIND= ni)        ::n
x = fp(0.375_nr) !es el maximo de la f. por lo tanto la cota del epsilon la va a tener el.
n=5
do 
    if ((x+10._nr**n) -x  == 0) exit
    n = n -1
end do

write(*,*) "epsilon es 10**", n

end program