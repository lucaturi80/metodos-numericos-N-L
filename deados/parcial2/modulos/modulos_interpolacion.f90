module modulos2

use global_prec

implicit none


contains

!*********************************************************************************
! metodo de interpolacion de lagrange
!*********************************************************************************


subroutine lagrange(n,x0,y0,x,y)

implicit none
!*********************************************************************************************
! n = numero de puntos que conocemos de la funcion menos 1, grado del polinomio de lagrange
! x0 = vector con los valores en x que conocemos
! y0 = vector con los valores en y que conocemos
! x = punto en que queremos aproximar
! Y = valor del polinomio de lagrange en el x
!*********************************************************************************************

integer(kind=id),parameter :: ed=il, dp=rd

!defino variables

integer(kind=ed), intent(in)            :: n
real(kind=dp),intent(in),dimension(0:n) :: x0,y0
real(kind=dp),intent(in)                :: x
real(kind=dp),intent(out)               :: y
!variables auxiliares
real(kind=dp)    :: p
integer(kind=ed) :: i,j

!programa

y = 0.0_dp

do i=0,n 

  p = 1.0_dp !el p va a servir para acumular el producto de los polinomios de lagrange
  
   do j=0,n                                              ! hago un loop en el que construye
      if(j /= i) p = p * (x - x0(j)) / (x0(i) - x0(j))   ! la parte del pol de lagrange en la que hace el
   end do                                                ! producto de (x - xi)/(xk - xi)
   
   y = y + y0(i) * p ! hago la parte del producto por la funcion en xi para construir 
                     ! los terminos del polinomio de lagrange y los voy sumando en el 
                     ! do para tener el polinomio completo evaluado en x
   
end do

end subroutine lagrange

!*********************************************************************************
! metodo de interpolacion de newton
!*********************************************************************************

subroutine newton(n,x0,y0,x,y)

implicit none
!*********************************************************************************************
! n = numero de puntos que conocemos de la funcion menos 1, grado del polinomio de newton
! x0 = vector con los valores en x que conocemos
! y0 = vector con los valores en y que conocemos
! x = punto en que queremos aproximar
! Y = valor del polinomio de newton en el x
!*********************************************************************************************

integer(kind=id),parameter :: ed=il, dp=rd

!defino variables

integer(kind=ed), intent(in)            :: n
real(kind=dp),intent(in),dimension(0:n) :: x0,y0
real(kind=dp),intent(in)                :: x
real(kind=dp),intent(out)               :: y

!defino variable auxiliares

real(kind=dp),dimension(0:n) :: dif, prod
integer(kind=ed)  :: i,j

!inicializamos en vector de la diferencia dividida con los valores de y
do i=0,n
   dif(i) = y0(i)
end do

!hacemos un loop que  nos va a guardar el valor de la diferencia dividida de orden n en el vector 
do    j = 1,n                                            ! dif(:), para j=1, va a escribir en la 
      do i = n ,j , -1                                   ! coordenada i-esima las diferencias divididas
              dif(i) = (dif(i)-dif(i-1))/(x0(i)-x0(i-j)) ! 1eras, ya que el vector esta inicialisado
      end do                                             ! entonces para j=2 la cordenada 1 no se tocara
                  ! y escribira la dif de orden 2 en todo el resto, asi cuando haga todas las j, quedara
end do            ! la diferencia dividida de orden i en dif(i)

!armo un vector con el producto de x-xi en cada coordenada

prod(0) = 1.0_dp
                                  ! en 0 vale 1, en 1 vale (x-x0),en 2 vale (x-x0)*x(x-x1), y asi
do i=1,n                              ! en n vale (x-x0)*(x-x1)*...*(x-x(n-1))
   prod(i) = prod(i-1)*(x-x0(i-1))
end do

!armo el polinomio de newton evaluado en x

y = 0.0_dp

do i=0,n
  y = y + dif(i)*prod(i)
end do

end subroutine newton

end module modulos2


