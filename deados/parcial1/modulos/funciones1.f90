module funciones
use isoprecisiones
CONTAINS

real(kind=dp) FUNCTION f1(x)
real(kind=dp),intent(in)        ::x
f1 = cos(x**2 - 0.5)
end FUNCTION f1

real(kind=dp) FUNCTION f2(x)
real(kind=dp),intent(in)        ::x
f2 = (-sin(x**2 - 0.5)) * (2*x)
end FUNCTION f1

end module