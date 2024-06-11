MODULE funciones
use univ_const
implicit none
REAL(KIND=nr)         ::gamma = 0.1_nr, beta = 0.5_nr
REAL(KIND=nr)         ::N = 1000
contains

FUNCTION f(t, w)
REAL(KIND = nr), intent(in) 		            ::t
REAL(KIND = nr), dimension(:), intent(in)       ::w
REAL(KIND=nr),dimension(size(w))                ::f
f(1) = -(beta*w(1)*w(2))/N 
f(2) = (beta*w(1)*w(2))/N - gamma * w(2)
f(3) = gamma * w(2)
END FUNCTION f 
END MODULE