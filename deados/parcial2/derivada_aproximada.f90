module derivada_aprox
use variables_prec

INTEGER(kind = il), PARAMETER     :: ni = il, nr = rd


contains
!-----------------------------------------------
! aclaracion:   fxis(i)= f(x0 + h*(i-1))  
!-----------------------------------------------


!derivada de f usando formula de 2 puntos, con x al extremo izquierda
REAL(KIND=nr) FUNCTION dF2I(h, fx1, fx2)
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2
dF2I = (fx2-fx1) / h
end function dF2I

!derivada de f usando formula de 2 puntos, con x al extremo derecha
REAL(KIND=nr) FUNCTION dF2D(h, fx1, fx2)
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2
dF2D = -(fx2-fx1) / h
end function dF2D

!derivada de f usando formula de 3 puntos, con x al extremo izquierda
REAL(KIND=nr) FUNCTION dF3I(h, fx1, fx2, fx3)  !(fx1,fx2,fx3) = (f(x),f(x+h), f(x+2h))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3
dF3I = (-3*fx1 + 4*fx2 - fx3) / (2*h)
end function dF3I

!derivada de f usando formula de 3 puntos centrada
REAL(KIND=nr) FUNCTION dF3C(h, fx1, fx2, fx3)  !(fx1,fx2,fx3) = (f(x-h),f(x), f(x+h))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3
dF3C = (fx3-fx1) / (2*h)
end function dF3C

!derivada de f usando formula de 3 puntos, con x extremo derecha
REAL(KIND=nr) FUNCTION dF3D(h, fx1, fx2, fx3) !(fx1,fx2,fx3) = (f(x-2h),f(x-h), f(x))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3
dF3D = -(-3*fx3 + 4 * fx2 - fx1) / (2*h)
end function dF3D

!derivada de f usando formula de 5 puntos con x extremo izquierda
REAL(KIND=nr) FUNCTION dF5I(h, fx1, fx2, fx3, fx4, fx5) !(fx1,fx2,fx3,fx4,fx5)=(f(x),f(x+h),f(x+2h),f(x+3h),f(x+4h))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3,fx4,fx5
dF5I = (-25*fx1+ 48*fx2 - 35*fx3 + 16*fx4 - 3*fx5) / (12*h)
end function dF5I

!derivada de f usando formula de 5 puntos centrada
REAL(KIND=nr) FUNCTION dF5C(h, fx1, fx2, fx3, fx4, fx5) !(fx1,fx2,fx3,fx4,fx5)=(f(x-2h),f(x-h),f(x),f(x+h),f(x+2h))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3,fx4,fx5
dF5C = (fx1 - 8*fx2 + 8*fx4 - fx5) / (12*h)
end function dF5C

!derivada de f usando formula de 5 puntos con x extremo derecha
REAL(KIND=nr) FUNCTION dF5D(h, fx1, fx2, fx3, fx4, fx5) !(fx1,fx2,fx3,fx4,fx5)=(f(x-4h),f(x-3h),f(x)-2h,f(x-h),f(x))
REAL(KIND=nr),intent(in)                   :: h, fx1,fx2,fx3,fx4,fx5
dF5D = -(-25*fx5+ 48*fx4 - 35*fx3 + 16*fx2 - 3*fx1) / (12*h)
end function dF5D

end module