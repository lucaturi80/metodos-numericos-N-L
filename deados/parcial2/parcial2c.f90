program 1c

use global_prec
use derivada_aprox
use funciones

!----------------------------------
!declaracion de variables
!----------------------------------
implicit none

INTEGER(KIND = ni) i
REAL(KIND = nr) rmin, rmax, rvar
REAL(KIND = nr) dfx, EAdf

open(unit = 10, file = "data/parcial2c.dat")
write(10,*) "r, dv/dr ( r), EAX comparandolo con la derivada analiticamente"
!----------------------------------

!----------------------------------
!proceso e impresion de datos
!----------------------------------

rmin = 0.36_nr!nm
rmax = 0.75_nr!nm


rvar = rmin
DO 
    IF (rvar = rmin) THEN
        dfx = dF2I(0.005_nr, fp(rvar), fp(rvar+0.005_nr))
    ELSE IF (rvar = rmax) THEN
        dfx = dF2D(0.005_nr, fp(rvar-0.005_nr), fp(rvar))
    ELSE 
        dfx = dF3C(0.005_nr, fp(rvar-0.005), fp(rvar), fp(rvar+0.005))
    END IF
    EAdf = abs( dfp(rvar) - dfx )
    write(10,66) rvar, dfx, EAX
    if (rvar = rmax) exit

END DO

!----------------------------------
close(10)
end program