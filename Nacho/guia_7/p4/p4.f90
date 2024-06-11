PROGRAM prg

use Eq_diff

!-----------------------------
!definicion de variables
!-----------------------------
implicit none
INTEGER(KIND = ni)              :: i, u, inciso
REAL(KIND=nr)                   :: t, ti, tf, h
REAL(KIND=nr), dimension(2)     :: w
REAL(KIND=nr), dimension(2,2)   :: w0
ti = 0._nr
tf = 10.0_nr
h  = 0.01_nr
w0(1,:) = (/0.5_nr, 0.0_nr/)
w0(2,:) = (/0.25_nr, 0.0_nr/)

open(unit=10, file="data/datosa.dat")
open(unit=20, file="data/datosb.dat")

!-----------------------------

!-----------------------------
!proceso
!-----------------------------
DO inciso =1,2
    u = 10*inciso
    i=1
    t=ti
    w = w0(inciso,:)
    write(u,66) t, w(:)
    DO WHILE (t <= tf-h)
        call rk4(h, w, t)
        t = ti + i*h
        write(u,66) t, w(:)
66 format (3(F19.9))
        i=i+1
    END DO
END DO
!-----------------------------


END PROGRAM