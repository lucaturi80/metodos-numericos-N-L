program parcial2e 

use modulos2
use funciones

implicit none

!defino variables
real(nr)                            :: r0,r,Vr04,Vr08,Vr0,Er
integer(ni)                         :: nu1,nu2,nu3,n_ite,e,n,i
real(nr),allocatable,dimension(:)   :: x0,y0

!voy a usar el polinomio de lagrange completo para interpolar el r en cada punto

open(newunit=nu1,file='data/datos.dat',status='old',action='read')
open(newunit=nu3,file='data/parcial2e.dat',status='replace',action='write')

write(nu3,'(2A18)') '# r (nm)','V(r) (erg)'

!guardo todos los datos del archivo de datos en un vector para darle al polinomio de lagrange para que interpole los r

n = 0_ni
do 
  read(nu1,*,iostat=e) r   ! leo el numero de datos del archivo
  if(e/=0) exit
  n = n + 1_ni
end do

rewind(nu1)

allocate(x0(n),y0(n))

do i=1,n
  read(nu1,*) x0(i),y0(i)
end do

!ahora interpolo en cada punto y lo guardo en el archivo de salida

r = 0.32_nr

do
  call lagrange(n-1,x0,y0,r,Vr0)  ! llamo a lagrange para cada r desde 0.36 a 0.75 de a 0.005
  write(nu3,'(2F18.8)') r,Vr0
  r = r + 0.005_nr
  if(r > 0.75_nr) exit
end do

deallocate(x0,y0)  !desasigno memoria para poder usar los vectores en los siguientes pasos
rewind(nu1)
!el grafico esta en el archivo parcial2e.gp, se grafica al ejecutar el run_e.sh


!ahora voy a usar el polinomio de lagrange de grado 3 usando los 4 puntos mas cercanos al r que queramos calcular
open(newunit=nu2,file='data/parcial2e_mejorado.dat',status='replace',action='write')


write(nu2,'(2A18)') '# r (nm)','V(r) (erg)'

n_ite = 0_ni
r0 = 0.320_nr

allocate(x0(0:3),y0(0:3))

do                       ! en este do voy a estar evaluando en cada r de los que dice el enunciado el polinomio de lagrange
                         ! de orden 3 usando 2 datos menores que r0 y 2 mayores, es lo que hacia en el punto d, pero aplicado a 
  n_ite = n_ite + 1_ni   ! cada r en este intervalo
  n = 0_ni               !
  

  do
     read(nu1,*,iostat=e)r     ! esto nos dice en que pocision del archivo esta el primer valor mayor que r0
     if(r >= r0) exit          ! y lo guarda en la variable n
     n = n + 1_ni
     if(e /= 0) exit     ! si iostat es /=0 quiere decir que fallo la lectura de datos o se termino el archivo
  end do

  rewind(nu1)           ! vuelvo al inicio del archivo de datos

  do i=1,n-3          ! esto mueve el archivo asta que esta en la pocision que queremos para que lea 
     read(nu1,*) r    ! los 2 datos de antes de r0 y los 2 de despues
  end do



  do i=0,3                    ! esto lee los 2 datos antes de r0 y los 2 de despues y los guarda en un
     read(nu1,*) x0(i),y0(i)  ! en 2 vectores para darle al modulo de interpolacion de lagrange
  end do

  call lagrange(3,x0,y0,r0,Vr04)  ! evaluamos en lagrange de orden 3 los 4 datos
  !print*, Vr04

  write(nu2,'(2F18.8)') r0,Vr04  ! escribo en el archivo de salida los nuevos datos

  if(r0 >= 0.735_nr) exit   ! condicion de salida, si r0 > 0.735 se terminaron los datos que podemos evaluar de esta manera 
                            ! ya que el ultimo dato no tiene datos mas adelante
  r0 = r0 + 0.005_nr  ! adelanto r0 en 1 paso

  rewind(nu1)
 
end do

!ahora evaluo la ultima de forma munual ya que no tiene datos mayores, es como una iteracion
!*************************************************************************
n_ite = n_ite + 1_ni
rewind(nu1)
r0 = r0 + 0.005_nr

do i=1,n-3
    read(nu1,*) r
end do
  
  
do i=0,3                    
      read(nu1,*) x0(i),y0(i) 
end do
  
call lagrange(3,x0,y0,r0,Vr04)
!print*, Vr04
  
write(nu2,'(2F18.8)') r0,Vr04

deallocate(x0,y0)
!******************************************************************************************

! en el archivo para correr el programa esta incluido la llamada al script de gnuplot que grafica los resultados junto con 
! la funcion ajustada con gnplot

end program parcial2e
