module Eq_diff

!------------------------------------
!precision
!------------------------------------
!use isoprec 
use funciones

!INTEGER(KIND=il),parameter ::ni=il, nr=rd
!------------------------------------


CONTAINS
SUBROUTINE paso_integracion(metodo,h,w,t)
implicit none
INTEGER(KIND = ni), intent(in)                  :: metodo
real(kind = nr), intent(in)                     :: h, t
REAL(KIND = nr), dimension(:), intent(inout)    :: w
SELECT CASE (metodo)
    CASE (1)
        CALL euler(h,w,t)
    CASE (2)
        CALL rk2(h,w,t)
    CASE (3)
        CALL rk4(h,w,t)
END SELECT
END SUBROUTINE

SUBROUTINE euler(h, w, t)
implicit none
real(kind = nr), intent(in)                     :: h, t
REAL(KIND = nr), dimension(:), intent(inout)    :: w
w = w + h * f(t,w)
end SUBROUTINE


SUBROUTINE rk2(h, w, t)
implicit none

real(kind = nr), intent(in)                     :: h, t
REAL(KIND = nr), dimension(:), intent(inout)    :: w
REAL(KIND = nr), dimension(size(w))             :: k
k = h * f(t,w)

w = w + h * f(t + h*0.5_nr , w + k * 0.5_nr)
end SUBROUTINE

SUBROUTINE rk4(h, w, t)
implicit none
real(kind = nr), intent(in)                     :: h, t
REAL(KIND = nr), dimension(:), intent(inout)    :: w
REAL(KIND = Nr), dimension(size(w))             :: k1, k2, k3, k4
k1 = h * f(t,w)
k2 = h * f(t + h * 0.5_nr, w + k1 * 0.5_nr)
k3 = h * f(t + h * 0.5_nr, w + k2 * 0.5_nr)
k4 = h * f(t + h, w + k3)

w = w + (k1 + 2._nr * k2 + 2._nr * k3+ k4) / 6._nr
end SUBROUTINE
end module
