program aprox
use isoprecisiones
use ls
use funciones

implicit none
INTEGER(KIND = il) 	::datos_u = 1
INTEGER(KIND=il) 	::total = 4
REAL, DIMENSION(total,2)	 	::xv

open(unit = datos_u, file = "data/datos.dat", status = "old", action = "read")
do i = 1, total
	read(datos_u,*) daz,xv(i,1), xv(i,2)
end do
open(unit = 2, file = "data/funcion.dat")



do x = 1,100

write(2,66) x/10, f(x/10)
66 FORMAT (2(F2.2))

end do

close(datos_u)
close(2)

end program aprox
