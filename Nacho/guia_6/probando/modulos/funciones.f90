module funciones 
!-----------------------------------------
!precision
!-----------------------------------------
use isoprec
!INTEGER(KIND=il),parameter ::ni=il
INTEGER(KIND=il),parameter ::fnr=rd
!-----------------------------------------
contains

REAL(KIND = fnr) FUNCTION f(x)
REAL(KIND = fnr), intent(in) 		::x
f = sin(x)
END FUNCTION f

REAL(KIND = fnr)  FUNCTION prim_f(x)
REAL(KIND = fnr), intent(in)         ::x
prim_f = -cos(x)
END FUNCTION prim_f

end module