PROGRAM p4
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

INTEGER(KIND=pni)                        :: n, inciso
INTEGER(KIND=pni)                        :: metodo,j
INTEGER(KIND=Ni)                         ::uintegral,uerror
REAL(KIND=pnr)                           ::a,b,h, integralvar, integral0
REAL(KIND=pnr),dimension(3)              ::integral , er
open(unit = 10, file = "data/aintegral.dat")
open(unit = 20, file = "data/aerror.dat")
open(unit = 30, file = "data/bintegral.dat")
open(unit = 40, file = "data/berror.dat")
!---------------------------
!PROCESO
!---------------------------
do inciso= 1,2
uintegral = 10 + 20*(inciso-1)
uerror = 20 + 20*(inciso-1)
select case(inciso)
    case(1)
        a = 0._pnr
        b = 0.5_pnr
        integral0 = prima(b)-prima(a)
    case(2)
        a = 1.0_pnr
        b = 1.5_pnr
        integral0 = primb(b)-primb(a)
end select
write(uintegral,50) "n", "integral_puntomedio", "integral_trapecio", "integral_simpson"
write(uerror,50) "n", "error_puntomedio", "error_trapecio", "error_simpson"
50 format (A4,(3(x,A18)))
do n = 2,100,2
h = (b-a) / n

do metodo = 1,3
    select case (inciso)
        case (1)
            call calc_integral(metodo,n,fa,a,b,integralvar)
        case (2)
            call calc_integral(metodo,n,fb,a,b,integralvar)
    end select
    integral(metodo) = integralvar
    !write(*,*) integralvar
    er(metodo) = abs(integral(metodo) - integral0)
end do
write(uintegral,66) n, integral(:)
write(uerror,66) n, er(:)
66 FORMAT (I4, 3(x,E18.9))
end do
end do

!---------------------------


END PROGRAM
