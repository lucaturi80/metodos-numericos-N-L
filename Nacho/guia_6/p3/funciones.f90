module funciones 
!-----------------------------------------
!precision
!-----------------------------------------
use univ_const
!use isoprec
!!INTEGER(KIND=il),parameter ::ni=il
INTEGER(KIND=il),parameter ::fnr=rd
!-----------------------------------------
contains

REAL(KIND = fnr) FUNCTION f1(x)
REAL(KIND = fnr), intent(in) 		::x
f1 = exp(-x)
END FUNCTION f1


end module