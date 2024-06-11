MODULE funciones
use univ_const
implicit none
contains

FUNCTION f(t, w)
REAL(KIND = nr), intent(in) 		            ::t
REAL(KIND = nr), dimension(:), intent(in)       ::w
REAL(KIND=nr),dimension(size(w))                ::f
f(1) = -w(1) + sin(2._nr * pi * t)
END FUNCTION f 

FUNCTION y(t)
REAL(KIND = nr), intent(in) 		            ::t
REAL(KIND=nr),dimension(1)                      ::y
y(1) = (1 + (2*pi)/(1+4*pi**2)) * e **(-t) + (sin(2*pi*t)-2*pi*cos(2*pi*t))/(1+4*pi**2)
!      (1 + (2*pi)/(1+4*pi**2)) * e **(-x) + (sin(2*pi*x)-2*pi*cos(2*pi*x))/(1+4*pi**2)
END FUNCTION y

END MODULE