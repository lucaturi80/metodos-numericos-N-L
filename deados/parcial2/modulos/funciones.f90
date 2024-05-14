module funciones

use global_prec

implicit none
REAL(KIND=nr)               :: prfpot= 125.7380_nr !profundidad del potencial (ajustando la funcion a los datos)
REAL(KIND=nr)               :: sigma = 0.334454_nr !la distancia a la cual el potencial entre las dos partıculas es cero.(ajustando la funcion a los datos)

contains

!formula del potencial
REAL(KIND = nr) FUNCTION fp(r)
REAL(KIND=nr), intent(in)   ::r 
fp = 4 * prfpot * ( (sigma/r)**12 - (sigma/r)**6)
END FUNCTION fp

!derivada del potencial
REAL(KIND = nr) FUNCTION dfp(r)
REAL(KIND=nr), intent(in)   ::r 
dfp = 4 * prfpot * ( (sigma**12)*((-12)*r**(-13)) - (sigma**6)*((-6)* r**(-7)))
END FUNCTION dfp

end module
