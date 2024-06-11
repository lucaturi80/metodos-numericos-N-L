PROGRAM prg

use Eq_diff

!-----------------------------
!definicion de variables
!-----------------------------
implicit none
INTEGER(KIND = ni)              :: i, inciso
REAL(KIND=nr)                   :: t, ti, tf, h
REAL(KIND=nr), dimension(3)     :: w, w0
ti = 0._nr
tf = 200.0_nr
h  = 1.0_nr
w0 = (/990._nr, 10.0_nr, 0._nr/)

open(unit=10, file="data/datos.dat")

!-----------------------------

!-----------------------------
!proceso
!-----------------------------
i=1
t=ti
w = w0
write(10,66) t, w(:)
DO WHILE (t <= tf-h)
    call rk4(h, w, t)
    t = ti + i*h
    write(10,66) t, w(:)
66 format (4(F19.9))
    i=i+1
END DO

!-----------------------------


END PROGRAM