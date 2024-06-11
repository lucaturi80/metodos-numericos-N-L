module funciones
use precsn

contains

! __________________________________________
function funcion ( x )

implicit none

real ( kind = wp ) , intent ( in ) :: x
real ( kind = wp ) :: funcion


funcion = exp(-x)

end function funcion


end module funciones
