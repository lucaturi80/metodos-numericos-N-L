program parcial2d

use modulos2

implicit none

!inferencia sobre el valor del potencial de lennard-Jones en r0 = 0.4375 interpolando

!defino variables

real(nr)              :: r0 = 0.4375,r
integer(ni)           :: nu,e,n
real(nr),dimension(0:3) :: x0,y0

! usando 2 puntos a derecha y 2 a izquierda

open(newunit=nu,file='data/datos.dat',status='old',action='read')

n = 0_ni

do
 read(nu,*,iostat=e)r     ! esto nos dice ne que pocicion del archivo esta el primer valor mayor que r0
 if(r > r0) exit 
 n = n + 1_ni
 if(e == -1) print*, 'el valor r0 no se puede interpolar con los datos del archivo'
end do

rewind(nu)

do i=1,n-3
 read(nu,*) r
end do

do i=0,3
    read(nu,*) x0(i),y0(i)

end program parcial2d
