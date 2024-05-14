program parcial2d

use modulos2

implicit none

!inferencia sobre el valor del potencial de lennard-Jones en r0 = 0.4375 interpolando

!defino variables

real(nr)                            :: r0 = 0.4375,r,Vr04,Vr08,Vr0,Er
integer(ni)                         :: nu,e,n,i
real(nr),allocatable,dimension(:)   :: x0,y0
real(nr)                            :: Vr0a = -80.35007078_nr

! usando 2 puntos a derecha y 2 a izquierda
!*******************************************************************************************************************************

open(newunit=nu,file='data/datos.dat',status='old',action='read')

n = 0_ni

do
 read(nu,*,iostat=e)r     ! esto nos dice e que pocicion del archivo esta el primer valor mayor que r0
 if(r > r0) exit          ! y lo guarda en la variable n
 n = n + 1_ni
 if(e == -1) print*, 'el valor r0 no se puede interpolar con los datos del archivo'  ! si iostat es <0 quiere decir que se alcanzo el final del archivo
end do

rewind(nu)

do i=1,n-2  !leo y tiro la cantidad de datos suficiente hasta que me queden justo los 2 puntos anteriores a r0
 read(nu,*) r
end do

allocate(x0(0:3),y0(0:3))   ! asigno los grados de los vectores en base a la cantidad de datos que quiero usar

do i=0,3                    ! esto lee los 2 datos antes de r0 y los 2 de despues y los guarda
    read(nu,*) x0(i),y0(i)  ! en 2 vectores para darle al modulo de interpolacion de lagrange
!    write(*,*) x0(i)        !corroboro en la consola q las 4 x son las dos anteriores y dos posteriores a r0
end do

call lagrange(3,x0,y0,r0,Vr04)

Er = abs(Vr04-Vr0a) / abs(Vr0a)

write(*,'(A48,F18.8)') "valor del potencial de lennard-jones en el punto",r0
write(*,'(A100,F18.8)') "interpolado usando polinomio de lagrange y los 4 puntos mas proximos a ro"
write(*,'(A17,F18.8)') "el potencial vale:",Vr04
write(*,'(A26,F18.8)') "y el error relativo seria:",Er
print*

deallocate(x0,y0)  ! desasigno el grado a los vectores para poder usarlos en la proxima parte
rewind(nu)

!usando 4 puntos a derecha y 4 a izquierda
!*******************************************************************************************************************************

allocate(x0(0:7),y0(0:7))  ! asigno grado a los vectores para usar 8 puntos

do i=1,n-4
    read(nu,*) r !leo y tiro la cantidad de datos suficiente hasta que me queden justo los 4 puntos anteriores a r0
end do

do i=0,7                    ! esto lee los 4 datos antes de r0 y los 4 de despues y los guarda en un
    read(nu,*) x0(i),y0(i)  ! en 2 vectores para darle al modulo de interpolacion de lagrange
!    write(*,*) x0(i)        !corroboro en la consola q las 4 x son las dos anteriores y dos posteriores a r0
end do

call lagrange(7,x0,y0,r0,Vr08)

Er = abs(Vr08-Vr0a) / abs(Vr0a)

write(*,'(A48,F18.8)') "valor del potencial de lennard-jones en el punto",r0
write(*,'(A100,F18.8)') "interpolado usando polinomio de lagrange y los 8 puntos mas proximos a ro"
write(*,'(A17,F18.8)') "el potencial vale:",Vr08
write(*,'(A26,F18.8)') "y el error relativo seria:",Er
print*

deallocate(x0,y0)
rewind(nu)

!ahora usando todos los datos del archivo
!*******************************************************************************************************************************

n = 0_ni
do
 read(nu,*,iostat=e)  ! leo la cantidad de datos que tiene el archivo para asignar memoria a los vectores
 if(e /= 0) exit
 n = n + 1_ni
end do 

allocate(x0(0:n-1),y0(0:n-1))  ! les asigno espacion en base al numero de datos
rewind(nu)

do i=1,n                           ! esto lee todos los datos del archivo y los guarda en
    read(nu,*,iostat=e) x0(i-1),y0(i-1)  ! 2 vectores para darle al modulo de interpolacion de lagrange
    !print*, e
end do

call lagrange(n-1,x0,y0,r0,Vr0)

Er = abs(Vr0-Vr0a) / abs(Vr0a)

write(*,'(A48,F18.8)') "valor del potencial de lennard-jones en el punto",r0
write(*,'(A100,F18.8)') "interpolado usando polinomio de lagrange y todos los puntos del archivo"
write(*,'(A17,F18.8)') "el potencial vale:",Vr0
write(*,'(A26,F18.8)') "y el error relativo seria:",Er


end program parcial2d
