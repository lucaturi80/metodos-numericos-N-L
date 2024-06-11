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
INTEGER(KIND=pni)                        :: metodoi,j
REAL(KIND=pnr)                           ::a=0._pnr,b=1._pnr,h, integralvar, integral0
REAL(KIND=pnr), dimension(3,2)           ::integral,er
CHARACTER(len=7) ,dimension(3)  :: metodo = ["puntomd", "trapeci", "simpson"]

integral0 = 1._nr - exp(-1._nr)
!---------------------------
!PROCESO
!---------------------------

do j = 1,2 !para variar las h
    h = 0.05_pnr / j !para probar con h = 0.05 y h = 0.025
    n = (b-a) / h
    write(*,*) "para h = ", h
        do metodoi = 1,3 !para variar los metodos
            call calc_integral(metodoi, n,f1, a, b, integralvar)
            integral(metodoi,j) = integralvar
            write(*,*) "para el metodo ",metodo(metodoi)," la integral es", integral(metodoi,j)
            er(metodoi,j) = abs(integral(metodoi,j) - integral0)
            write(*,*) "su error es", er(metodoi,j)
        end do
end do


!ver cociente de errores
do  metodoi = 1,3
write(*,*) "para el metodo ",metodo(metodoi)," el cociente entre los errores es", er(metodoi,1)/er(metodoi,2)
end do 


!---------------------------


END PROGRAM
