module interpolantes

contains

subroutine lagrange(n,x,fx,x0,px0)
use isoprec

INTEGER(kind= il), PARAMETER   ::ni = il
INTEGER(kind= il), PARAMETER   ::nr = rd

INTEGER(kind= ni),intent(in)                       :: n
REAL(kind=nr), dimension(n+1), intent(in)          :: x
REAL(kind=nr), dimension(n+1), intent(in)          :: fx
REAL(kind=nr),  intent(in)                         :: x0
REAL(kind=nr),  intent(out)                        :: px0
REAL(Kind=nr)                 :: num, denm
REAL(kind=nr), dimension(n+1) :: L
INTEGER(kind=ni)              :: i,j

do j=1,n+1
num = 1
denm= 1
    do i = 1,j-1 !defino numerador y denominador
        num = num * (x0-x(i))
        denm = denm * (x(j)-x(i))
    end do
    do i = j+1,n+1 !esquivo el xsubk
        num = num * (x0-x(i))
        denm = denm * (x(j)-x(i))
    end do
    L(j) = num / denm  !igualo a la fraccion
end do

px0 = 0
do i = 1,n+1
    px0 = px0 + fx(i) * L(i) !defino el valor del polinomio evaluado en x0
end do
end subroutine


subroutine newton(n,x,fx,x0,px0)
use isoprec

INTEGER(kind= il), PARAMETER   ::ni = il
INTEGER(kind= il), PARAMETER   ::nr = rd

INTEGER(kind= ni),intent(in)                       :: n
REAL(kind=nr), dimension(n+1), intent(in)          :: x
REAL(kind=nr), dimension(n+1), intent(in)          :: fx
REAL(kind=nr),  intent(in)                         :: x0
REAL(kind=nr),  intent(out)                        :: px0
REAL(Kind=nr)                 :: prdcto_i
REAL(kind=nr), dimension(n+1, n+1) :: fcorchete !fcorchete(1,n) := f[x1,x2,..,xn]
INTEGER(kind=ni)              :: i,j


!inicializo la matriz
do i=1,n+1
do j=1,n+1
fcorchete(i,j) = 0
end do 
end do  

!trato de manera especial los f[xi] = fcorchete(i,i) = f(xi)
do i= 1,n+1
fcorchete(i,i) = fx(i) 
end do

!igualo f[xi,xi+1,...,xi+j] = (f[xi+1,...,xi+j] - f[xi,...,xi+j-1]) / (x(i+j)-x(i))
do j=1, n+1
    do i=1, n+1-j 
        fcorchete(i, i+j) = (fcorchete(i+1, i+j) - fcorchete (i, i+j-1)) / (x(i+j) - x(i)) 
    end do
end do

!evaluo el polinomio = f[x0]+ suma sub i hasta n ( f[x0,..,xi] (x-x0)..(x-xi-1))
px0 = 0
do i = 1, n+1
    !encuentro el producto de cada sumando
    prdcto_i = 1
    do j = 1, i - 1
        prdcto_i = prdcto_i * (x0 - x(j))
    end do
    px0 = px0 + fcorchete(1,i) * prdcto_i
end do

end subroutine

end module