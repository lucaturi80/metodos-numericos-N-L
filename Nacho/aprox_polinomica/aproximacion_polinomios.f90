program aprox
use isoprec
use ls
use funciones

implicit none
INTEGER(KIND = il) 						::x
INTEGER(KIND=il), PARAMETER 			::total = 2
REAL(KIND=rd), DIMENSION(total,2)	 	::xv
INTEGER(KIND = il) 						::daz, i

!leyendo los puntos de los que tengo informacion
open(unit = 1, file = "data/datos.dat", status = "old", action = "read")
do i = 1, total
	read(1,*) daz,xv(i,1), xv(i,2)
end do
open(unit = 2, file = "data/funcion.dat")


!ejecutando la funcion para algunos n 
do x = 1,100
	write(2,*) (x/10.0_rd), p(total, xv, x/10.0_rd)
	!66 FORMAT (2(F2.2))
end do

close(1)
close(2)

end program aprox
