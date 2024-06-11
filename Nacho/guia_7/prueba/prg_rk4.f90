PROGRAM prg

use Eq_diff

!-----------------------------
!definicion de variables
!-----------------------------
implicit none
INTEGER(KIND = ni)              :: i
REAL(KIND=nr)                   :: t, ti, tf, h
REAL(KIND=nr), dimension(2)     :: w, w0
ti = 0._nr
tf = 10.0_nr
h  = 0.2_nr
!w0(1) = 3.10_nr
!w0(2) = 0.00_nr
w0 = [3.10_nr, 0.0_nr]
open(unit=10, file="data/datos.dat")
!-----------------------------

!-----------------------------
!proceso
!-----------------------------
i=1
t=ti
w = w0
write(*,*) repeat("*",18)
write(10,66) t, w(1)
DO WHILE (t <= tf-h)
    call rk4(h, w, t)
    t = ti + i*h
    write(10,66) t, w(1)
66 format (3(F19.9))
    i=i+1
END DO
!-----------------------------


END PROGRAM