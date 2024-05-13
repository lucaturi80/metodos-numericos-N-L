module funciones

use global_prec

implicit none
REAL(KIND=nr)               ::prfpot = !profundidad del potencial (ajustando la funcion a los datos)
REAL(KIND=nr)               ::sigma = !la distancia a la cual el potencial entre las dos partıculas es cero.(ajustando la funcion a los datos)

contains

REAL(KIND = nr) FUNCTION V(r)
REAL(KIND=nr), intent(in)   ::r 
v = 4 * prfpot * ( (sigma/r)**12 - (sigma/r)**6)
END FUNCTION V

REAL(KIND = nr) FUNCTION V(r)
REAL(KIND=nr), intent(in)   ::r 
v = 4 * prfpot * ( (sigma**12)*((-12)*r**(-13)) - (sigma**6)*((-6)* r**(-7)))
END FUNCTION V

end module
