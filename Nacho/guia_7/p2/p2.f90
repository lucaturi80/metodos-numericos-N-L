PROGRAM prg

use Eq_diff

!-----------------------------
!definicion de variables
!-----------------------------
implicit none
INTEGER(KIND = ni)              :: i, ancho, metodo, u, j
REAL(KIND=nr)                   :: t, ti, tf, h
REAL(KIND=nr), dimension(1)     :: w, w0,y_t, er
ti = 0._nr
tf = 1.0_nr
w0 = y(ti)
open(unit=10, file="data/euler01.dat")
open(unit=20, file="data/euler005.dat")
open(unit=30, file="data/rk201.dat")
open(unit=40, file="data/rk2005.dat")
open(unit=50, file="data/rk401.dat")
open(unit=60, file="data/rk4005.dat")

!-----------------------------

!-----------------------------
!proceso
!-----------------------------
DO ancho = 1,2
    SELECT CASE (ancho)
        CASE(1)
            h = 0.01_nr
            u = -10
        CASE(2)
            h = 0.005_nr
            u=0
    END SELECT
    DO metodo= 1,3
        u = u+20
        i=1
        t=ti
        w = w0
        write(u,66) t, w(1)
        DO WHILE (t <= tf-h)
            call paso_integracion(metodo,h, w, t)
            t = ti + i * h
            y_t = y(t)
            do j = 1,size(er)
                er(j) = abs(w(j)-y_t(j))
            end do
            write(u,66) t, w(1), er
66 format (F5.3,2(3x,E13.7))
            i=i+1
        END DO
    END DO
END DO
!-----------------------------


END PROGRAM