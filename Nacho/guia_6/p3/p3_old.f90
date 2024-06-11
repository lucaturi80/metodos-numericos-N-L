PROGRAM p3 
use isoprec
use integrales
use funciones
implicit none
!-----------------------------------------
!precision
!-----------------------------------------
INTEGER(KIND=il),parameter ::pni=il, pnr=rd
!-----------------------------------------

!------------------------------------
!declaracion de variables
!------------------------------------

INTEGER(KIND=pni)                        :: n
INTEGER(KIND=pni)                        :: i,j
REAL(KIND=pnr)                           ::a=0._pnr,b=1._pnr,h, integralvar, integral0 = 0.632120559_pnr
REAL(KIND=pnr), dimension(3,2)           ::integral,er
REAL(KIND=pnr), allocatable, dimension(:)           ::x, fx,fxpm
CHARACTER(len=7) ,dimension(3)  :: metodo = ["puntomd", "trapeci", "simpson"]
write(*,*) "*"


!---------------------------
!PROCESO
!---------------------------
h = 0.05_pnr
do j = 1,2
h = h / j !para probar con h = 0.05 y h = 0.025
n = (b-a) / h 
allocate(x(0:n))
allocate(fx(0:n))

!llenando el vector
do i = 0,n
    x(i) = a + i*h
    fx(i) = f(a + i * h)
end do


!primero para trapecio compuesto
call trapecio(n,h,fx,integralvar)
integral(2,j) = integralvar
!despues para simpson
call simpson(n,h,fx,integralvar)
integral(3,j) = integralvar
!ahora para punto medio
!hay que cambiar los indices
n = n-2
allocate(fxpm(-1:n+1))
do i=-1,n+1
fxpm(i) = fx(i+1)
end do

call punto_medio(n,h,fx,integralvar)
integral(1,j) = integralvar

deallocate(x)
deallocate(fx)
deallocate(fxpm)
end do

h =0.05_pnr
!imprimir datos
do i = 1,2
write(*,*) "para h = ", h/i
do j = 1,3
write(*,*) "para el metodo ",metodo(j)," la integral es", integral(j,i)
er(j,i) = abs(integral(j,i) - integral0)
write(*,*) "su error es", er(j,i)

end do 
end do


!viendo errores
do j = 1,3
write(*,*) "para el metodo ",metodo(j)," el cociente entre los errores es", er(j,1)/er(j,2)
end do 


!---------------------------


END PROGRAM
