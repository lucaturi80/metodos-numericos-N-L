module funciones
use isoprec
use ls
contains

REAL (KIND= rd) FUNCTION p(total,xv,x)
INTEGER(KIND=il),intent(in)			                ::total
REAL(KIND=rd), DIMENSION(total,2), intent(in)	 	::xv
REAL (KIND = rd), intent(in)                        ::x
INTEGER(KIND=il)            		                ::n

p = 0
do n= 1, total
p = p + l(n,total, xv, x)* xv(i,2)
end do

end function p
end module funciones
