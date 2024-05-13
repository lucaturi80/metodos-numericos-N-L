program parcial2e 

use modulos2
use funciones

implicit none

!defino variables
real(nr)                            :: r0,r,Vr04,Vr08,Vr0,Er
integer(ni)                         :: nu1,nu2,n_ite,e,n,i
real(nr),allocatable,dimension(:)   :: x0,y0


!voy a usar el polinomio de lagrange de grado 3 usando los 4 puntos mas cercanos al r que queramos calcular

open(newunit=nu1,file='data/datos.dat',status='old',action='read')
open(newunit=nu2,file='data/parcial2e.dat',status='replace',action='write')

write(nu2,'(A4,2A18)') '#','r (nm)','V(r) (erg)'

n_ite = 0_ni
r0 = 0.320_nr

allocate(x0(0:3),y0(0:3))

do

 n_ite = n_ite + 1_ni
 n = 0_ni
  

 do
  read(nu1,*,iostat=e)r     ! esto nos dice ne que pocicion del archivo esta el primer valor mayor que r0
  if(r >= r0) exit 
  n = n + 1_ni
  if(e /= 0) exit
 end do

 rewind(nu1)

 do i=1,n-3
  read(nu1,*) r
 end do



 do i=0,3                    ! esto lee los 2 datos antes de r0 y los 2 de despues y los guarda en un
    read(nu1,*) x0(i),y0(i)  ! en 2 vectores para darle al modulo de interpolacion de lagrange
 end do

 call lagrange(3,x0,y0,r0,Vr04)
 print*, Vr04

 write(nu2,'(I4,2F18.8)') n_ite,r0,Vr04

 if(r0 >= 0.735_nr) exit

 r0 = r0 + 0.005_nr

 rewind(nu1)
 
end do

!ahora evaluo la ultima de forma munual ya que no tiene datos mayores

n_ite = n_ite + 1_ni
rewind(nu1)
r0 = r0 + 0.005_nr

do i=1,n-3
    read(nu1,*) r
end do
  
  
do i=0,3                    ! esto lee los 2 datos antes de r0 y los 2 de despues y los guarda en un
      read(nu1,*) x0(i),y0(i)  ! en 2 vectores para darle al modulo de interpolacion de lagrange
end do
  
call lagrange(3,x0,y0,r0,Vr04)
print*, Vr04
  
write(nu2,'(I4,2F18.8)') n_ite,r0,Vr04

deallocate(x0,y0)

!ahora grafico con gnuplot

call system ('gnuplot parcial2e.gp')

end program parcial2e