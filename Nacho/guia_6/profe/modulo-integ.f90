module inte_trape_simp

use precsn
use funciones

contains

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine puntomedio(integ ,a ,b , nn )

! ********************************************************************************************
! integral regla del punto medio
!
! integ = salida , resultado de la integral
! a = entrada , punto inferior del rango de integracion
! b = entrada , punto superior del rango de integracion
! nn = entrada , numero total de puntos; ( nn-1) = numero de intervalos  
! h = intrervalo , largo del intervalo = (b - a ) /( nn ) !!!  = (b - a ) /(2*(2**p ) )
!
! funcion ( x ) = se usa funcion definida en modulo funciones
!
! *******************************************************************************************


implicit none

integer , intent ( in ) :: nn

real ( kind = wp ) , intent ( out ) :: integ
real ( kind = wp ) , intent ( in ) :: a , b
real ( kind = wp )                 :: h , x

integer :: j

h = (b - a ) /( 1.0_wp * (nn-1))
print *, "h=", h
integ = 0._wp

x= 0.0_wp

do j = 0 , nn-2
    x =  a + h * ( j +  0.5_wp)
    integ = integ + funcion( x )
    print *,"x=",  x
end do

print *,"x + h =",  x + 0.5_wp * h

integ =  h * integ


end subroutine puntomedio



subroutine trapecio ( integ, a, b, nn )

! ********************************************************************************************
! integral regla de trapecio
!
! integ = salida , resultado de la integral
! a = entrada , punto inferior del rango de integracion
! b = entrada , punto superior del rango de integracion
! nn = entrada , numero total de puntos ; ( nn-1) = numero de intervalos
! h = intervalo , largo del intervalo = (b - a ) /( nn - 1) = (b - a ) /(2*(2** p ) )
!
! funcion ( x ) = se usa funcion definida en modulo funciones
!
! *******************************************************************************************


implicit none

integer , intent ( in ) :: nn

real ( kind = wp ) , intent ( out ) :: integ
real ( kind = wp ) , intent ( in ) :: a , b
real ( kind = wp )                 :: h , x

integer :: j

h = (b - a ) /( 1.0_wp *( nn-1) )

integ = 0._wp


do j = 2 , nn -1

x =  a + (j-1) * h

integ = integ + funcion ( x )

end do

integ = h *(0.5_wp *(funcion(a) + funcion (b)) + integ )


end subroutine trapecio



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



subroutine simpson ( integ , a, b, nn )

! *************************************************************************************************
! integral regla de Simpson
!
! integ = salida , resultado de la integral
! a = entrada , punto inferior del rango de integracion
! b = entrada , punto superior del rango de integracion
! nn = entrada , numero total de puntos ; ( nn -1) = numero de intervalos
! h = inteno , largo del intervalo = (b - a ) /( nn - 1) = (b - a ) /(2*(2** p ) )
!
! funcion ( x ) = se usa funcion definida en modulo funciones
!
! ************************************************************************************************

implicit none

integer , intent ( in ) :: nn

real ( kind = wp ) , intent ( out ) :: integ
real ( kind = wp ) , intent ( in ) :: a , b

real ( kind = wp ) :: h , integ1 , integ2 , x
integer :: j
print *, "el número de puntos es nn = ", nn
print *, " el número de intervaloes es nn-1 =", nn-1

if (mod(nn,2) == 0 ) then
  write (* ,*) " Error subroutine simpson : "
  write (* ,*) " el numero total de puntos nn debe ser impar "

  integ = 0.0_wp

end if 

print *, "a =", a
print *, "b =", b



  h = (b - a)/( 1.0_wp *(nn-1))
  
 print *, "h=", h
  
  integ1 = 0._wp
  integ2 = 0._wp

  ! Cálculo de  la integral, saltando de a 2, agrupando términos pares e impares

  do j = 1 , nn-2, 2      !!!!!!  términos     impares       x 4  !!!

      x = a + j * h 
      integ1 = integ1 + funcion(x)

  end do
  
 print *, "x =", x , "final impares"


  do j = 2 , nn-3 , 2     !!!!!  términos     pares        x 2  !!!
  

      x = a + j * h 
      integ2 = integ2 + funcion(x)

  end do

print *, "x =", x, "final pares"


  integ = h *((funcion(a) + funcion(b)) + 4._wp * integ1 + 2._wp * integ2 )/3._wp




end subroutine simpson

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module inte_trape_simp
