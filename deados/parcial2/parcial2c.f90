program parcial2c

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
    IF (rvar == rmin) THEN
        dfx = dF3I(0.005_nr, fp(rvar), fp(rvar+0.005_nr), fp(rvar+0.010_nr))
    ELSE IF (rvar == rmax) THEN
        dfx = dF3D(0.005_nr, fp(rvar-0.010_nr), fp(rvar-0.005_nr), fp(rvar))
    ELSE 
        dfx = dF3C(0.005_nr, fp(rvar-0.005), fp(rvar), fp(rvar+0.005))
    END IF
    EAdf = abs( dfp(rvar) - dfx )
    write(10,66) rvar, dfx, EAdf
66 format (2(F18.9),4x , E18.12)
    if (rvar >= rmax) exit
    rvar = rvar + 0.005_nr
END DO

!----------------------------------
close(10)
end program