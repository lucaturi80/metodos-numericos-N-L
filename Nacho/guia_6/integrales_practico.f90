module integrales
!-----------------------------------------
!precision
!-----------------------------------------
use isoprec
use funciones
!INTEGER(KIND=il),parameter ::ni=il, nr=rd
!-----------------------------------------

!METODOS DE INTEGRACION 
contains
!numeralizacion de metodos
SUBROUTINE calc_integral(metodo,n,f,a,b,integral)
!-----------------------------------------
!Declaracion de variables
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                     ::n, metodo
REAL(KIND=nr),intent(in)                        ::a,b
REAL(KIND=nr)                                   ::h
INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral
interface
    function f(x)
      real (KIND=8) :: f
      real (KIND=8), intent(in) :: x
    end function f
end interface
!-----------------------------------------

!-----------------------------------------
!llamado a metodos externos segun el numero
!-----------------------------------------
SELECT CASE (metodo)
    CASE (1)
        CALL punto_medio(n,f,a,b,integral)
    CASE(2)
        CALL trapecio(n,f,a,b,integral)
    CASE(3)
        call simpson(n,f,a,b,integral)
END SELECT
!-----------------------------------------
END SUBROUTINE

!METODO DE PUNTO MEDIO
SUBROUTINE punto_medio(n,f,a,b,integral)
!-----------------------------------------
!DECLARACION DE VARIABLES
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                     ::n
REAL(KIND=nr),intent(in)                        ::a,b
REAL(KIND=nr)                                   ::h, x
INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral
interface
    function f(x)
      real (KIND=8) :: f
      real (KIND=8), intent(in) :: x
    end function f
end interface
h = (b-a)/n
!-----------------------------------------

!-----------------------------------------
!PROCESO
!-----------------------------------------
integral = 0 
do i=0,n-1
    x = a+(i+0.5_nr)*h
    integral = integral + f(x)
end do
integral = integral * h 
!-----------------------------------------
END SUBROUTINE



!METODO DE TRAPECIO
SUBROUTINE trapecio(n,f,a,b,integral)
!-----------------------------------------
!DECLARACION DE VARIABLES
!-----------------------------------------
implicit none

INTEGER(KIND=ni),intent(in)                     ::n
REAL(KIND=nr),intent(in)                        ::a,b
REAL(KIND=nr)                                   ::h,x
INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral
interface
    function f(x)
      real(KIND = 8) :: f
      real (KIND=8), intent(in) :: x
    end function f
end interface
h = (b-a)/n
!-----------------------------------------

!-----------------------------------------
!PROCESO
!-----------------------------------------
integral = f(a) + f(b)
do i=1,n-1
    x = a+i*h
    integral = integral + 2 * f(x) 
end do
integral = integral * (h / 2)
!-----------------------------------------
END SUBROUTINE



!METODO DE SIMPSON
SUBROUTINE simpson(n,f,a,b,integral)
!-----------------------------------------
!DECLARACION DE VARIABLES
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                     ::n
REAL(KIND=nr),intent(in)                        ::a,b
REAL(KIND=nr)                                   ::h,x,coefi, s1 , s2
INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral
interface
    function f(x)
      real (KIND=8) :: f
      real (KIND=8), intent(in) :: x
    end function f
end interface
s1 = 0._nr
s2 = 0._nr
h = (b-a)/n
write(*,*) a,b,h
!-----------------------------------------

!-----------------------------------------
!PROCESO
!-----------------------------------------
do i = 1,n-1,2
    x = a+ i *h
    s1 = s1 + f(x)
end do
do i = 2,n-2,2
    x = a+i*h
    s2 = s2 + f(x)
end do
integral = h * (f(a)+f(b) + 4._nr * s1+ 2._nr * s2)/3._nr
write(*,*) "integral es" , integral

!integral = f(a) + f(b)
!do i=1,n-1
!    ! a los pares los mult por 2 y a los imp por 4
!    x = a+i*h
!    coefi = (1._nr+mod(i,2))*2._nr
!    integral = integral + (coefi) * (f(x))
!    !write(*,*) x, coef,  f(x)
!end do
!!write(*,*) 1._nr, 1._nr,  f(b)
!
!integral = h* integral / 3._nr
!-----------------------------------------
END SUBROUTINE

end module