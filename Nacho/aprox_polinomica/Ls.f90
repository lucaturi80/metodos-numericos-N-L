module ls

use isoprec

contains
 
REAL(KIND = rd)  FUNCTION l(n,total, xv, x)

INTEGER(KIND = il), intent(in) 	                    ::n
INTEGER(KIND = il), intent(in) 	                    ::total
REAL(KIND = rd), intent(in) 	                    ::x
REAL(KIND = rd)			 	                        ::num, denm
INTEGER(KIND = il) 			 	                    ::i
REAL(KIND=rd), DIMENSION(total,2), intent(in)	 	::xv


num = 1
denm = 1
do i=1,n-1
num = num * (x- xv(i,1))
denm = denm * (xv(n,1) - xv(i,1))
end do
do i=n+1,total
num = num * (x- xv(i,1))
denm = denm * (xv(n,1) - xv(i,1))
end do

l= num / denm

end function
 
end module ls
