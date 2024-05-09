module interpolantes

contains

subroutine cuadrados_minimos(m,x,fx,a0,a1)
use isoprec

INTEGER(kind= il), PARAMETER   ::ni = il
INTEGER(kind= il), PARAMETER   ::nr = rd

INTEGER(kind= ni),intent(in)                       :: m
REAL(kind=nr), dimension(m), intent(in)            :: x
REAL(kind=nr), dimension(m), intent(in)            :: fx
REAL(kind=nr),  intent(out)                        :: a0
REAL(kind=nr),  intent(out)                        :: a1

INTEGER(kind=ni)                     :: i,j,k
REAL( kind = nr)                     :: num, denm
REAL(Kind=nr), DIMENSION(3, 2)        :: Sxy 
!-----------------------------------------------------------
!Sxy(i,j) = Suma(x**(i-1) * y**(j-1)

!Sxy(1,2) = Suma( x**0 * y**1 ) = suma(y)
!Sxy(2,2) = Suma( x**1 * y**1 ) = suma(xy)
!Sxy(2,1) = Suma( x**1 * y**0 ) = suma(x)
!Sxy(3,1) = Suma( x**2 * y**0 ) = suma(x**2)
!-----------------------------------------------------------

do i = 1,3
    do j = 1,2
        Sxy(i,j) = 0
        do k = 1,m
            Sxy(i,j) = Sxy(i,j) + x(k)**(i-1) * fx(k)**(j-1)
        end do
    end do
end do

write(*,*) m, Sxy(2,1), Sxy(1,2), Sxy(3,1), Sxy(2,2)
! formula para el a0
num = Sxy(3,1) * Sxy(1,2) - Sxy(2,2) * Sxy(2,1)
denm= m * Sxy(3,1) - Sxy(2,1) ** 2
a0 = num / denm

!formula para el a1
num = m * Sxy(2,2) - Sxy(2,1) * Sxy(1,2)
denm= m * Sxy(3,1) - Sxy(2,1) ** 2
a1 = num / denm

write(*,*) "a0 = ", a0, ", a1 =", a1


end subroutine

end module