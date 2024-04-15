module funciones 

use isoprecisiones

contains
!**********************************************************
function f_a(x)
real(kind=rd), intent(in) :: x
real(kind=rd) :: f_a

f_a = x * 2.0_rd - tan(x)

end function

!**********************************************************

function f_b(x)
real(kind=rd), intent(in) :: x
real(kind=rd) :: f_b

f_b = x * x - 3.0_rd

end function

!**************************************************************

function df_a(x)

real(kind=rd), intent(in) :: x
real(kind=rd) :: df_a

df_a = 2.0_rd - 1.0_rd / (cos(x) * cos(x))

end function 

!**************************************************************

function df_b(x)

real(kind=rd), intent(in) :: x
real(kind=rd) :: df_b

df_b = 2.0_rd * x

end function

end module funciones
