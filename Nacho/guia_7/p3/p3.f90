PROGRAM prg

use Eq_diff

!-----------------------------
!definicion de variables
!-----------------------------
implicit none
INTEGER(KIND = ni)              :: i, caso, j
REAL(KIND=nr)                   :: t, ti, tf, h
REAL(KIND=nr), dimension(1)     :: w_var
REAL(KIND=nr), dimension(6)     :: w, w0
h = 0.1_nr
ti = 0._nr
tf = 10.0_nr
!α1 = 0.5, α2 = 2.0, α3 = π , α4 = 3.6 α5 = 5.5 y α6 = 2π
w0 =[0.5_nr, 2.0_nr, pi, 3.6_nr, 5.5_nr, 2._nr*pi]
open(unit=10, file="data/eq_diff.dat")
write(10,"(A5, 6(3x,A13))") "t", "caso 1", "caso 2", "caso 3",  "caso 4", "caso 5", "caso 6" 
!-----------------------------


!-----------------------------
!PROCESO
!-----------------------------
t=ti
i=1
DO caso = 1,6
    w(caso) = w0(caso)
END DO
write(10,66) t, w(:)

DO WHILE (t <= tf-h)
    DO caso = 1,6
        w_var(1) = w(caso)
        call rk4(h, w_var, t)
        w(caso) = w_var(1)
    END DO
    i=i+1
    t = ti + i * h
    write(10,66) t, w(:)
66 format (F6.3,6(3x,E14.7))
END DO
!-----------------------------

END PROGRAM