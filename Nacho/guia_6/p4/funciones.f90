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

REAL(KIND=nr) FUNCTION fa(x)
REAL(KIND = fnr), intent(in)         ::x
fa= 2/(x-4)
END FUNCTION fa

REAL(KIND=nr) FUNCTION fb(x)
REAL(KIND = fnr), intent(in)         ::x
fb = (x**2) * LOG(x)
END FUNCTION fb

REAL(KIND = fnr)  FUNCTION prima(x)
REAL(KIND = fnr), intent(in)         ::x
prima=  2 * LOG(abs(x-4))
END FUNCTION prima

REAL(KIND = fnr)  FUNCTION primb(x)
REAL(KIND = fnr), intent(in)         ::x
primb = (x**3._nr / 3._nr) * (LOG(x)-(1._nr/3._nr))
END FUNCTION primb 

end module