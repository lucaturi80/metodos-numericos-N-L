module funciones
use ISOprec
integer(kind=il), parameter     :: np = rd 

CONTAINS

real(kind=np) FUNCTION f1(x)
real(kind=np),intent(in)        ::x
f1 = tan(x) - 2 * x
end FUNCTION

real(kind=np) FUNCTION f2(x)
real(kind=np),intent(in)        ::x
f2 = x**2 - 3._np
end FUNCTION

real(kind=np) FUNCTION f3(x)
real(kind=np), intent(in)       ::x 
f3 = 2*X
end FUNCTION

end module