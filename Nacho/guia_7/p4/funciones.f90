MODULE funciones
use univ_const
implicit none
contains

FUNCTION f(t, w)
REAL(KIND = nr), intent(in) 		            ::t
REAL(KIND = nr), dimension(:), intent(in)       ::w
REAL(KIND=nr),dimension(size(w))                ::f
f(1) = w(2)
f(2) = -10._nr * sin(w(1))
END FUNCTION f 
END MODULE