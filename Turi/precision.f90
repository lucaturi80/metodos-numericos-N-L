module isoprecisiones 

use,intrinsic    :: ISO_FORTRAN_ENV, only: real64
use,intrinsic    :: ISO_FORTRAN_ENV, only: int64
use,intrinsic    :: ISO_FORTRAN_ENV, only: int32
use,intrinsic    :: ISO_FORTRAN_ENV, only: int16
use,intrinsic    :: ISO_FORTRAN_ENV, only: int8
use,intrinsic    :: ISO_FORTRAN_ENV, only: real32
use,intrinsic    :: ISO_FORTRAN_ENV, only: real128

integer(kind=int8),parameter :: is=int8, id=int16, il=int32, ix=int64
integer(kind=int8),parameter :: rs=real32, rd=real64, rx=real128
! s=simple, d= doble , l=large, x=extralarge

end module isoprecisiones


