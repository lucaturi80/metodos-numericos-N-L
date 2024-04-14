module metodos

use ISOprec
use funciones


CONTAINS

subroutine biseccion(f,aext,bext,tol_x,tol_y,p,fp,ite,max_ite,error_r_x,error_a_y,exito)
    implicit none
    INTEGER(KIND=int32), PARAMETER ::wp32=il
    INTEGER(KIND=int32), PARAMETER ::wpr=rd
    real(KIND=wpr)                   ::f
    REAL(KIND=wpr), intent(in)       :: aext
    REAL(KIND=wpr), intent(in)       :: bext
    REAL(KIND=wpr), intent(in)       :: tol_x
    REAL(KIND=wpr), intent(in)       :: tol_y
    REAL(KIND=wpr), intent(out)      :: p 
    REAL(KIND=wpr), intent(out)      :: fp 
    REAL(KIND=wpr), intent(out)      :: error_r_x
    REAL(KIND=wpr), intent(out)      :: error_a_y

    INTEGER(KIND=wp32), intent(in)    ::MAX_ITE
    INTEGER(KIND=wp32), intent(out)   ::ite

    LOGICAL, intent(out)             ::exito
    
    REAL(KIND=wpr)                   ::fa
    REAL(KIND=wpr)                   ::fb
    REAL(KIND=wpr)                   ::a
    REAL(KIND=wpr)                   ::b
    a=aext
    b=bext
    fa= f(a)
    fb= f(b)

    if(fa*fb > 0) then
        exito = .false.
    else
        open(unit=1,file="datos.dat")
        do ite=1,MAX_ITE
            p = a + (b-a)/2.
            fp= f(p)

            if (fa*fp < 0) then
                b = p
                fb=f(b)
            else
                a = p
                fa=f(a)
            end if
            error_r_x = ABS(b-a)/ABS(b+a)
            error_a_y = ABS(fp)
            
            write(1,66) ite,p,fp,error_r_x,error_a_y
            write(*,66) ite,p,fp,error_r_x,error_a_y
66          FORMAT(I4, 4(2x,E18.12))

            if( error_r_x < tol_x  .and.  error_a_y<tol_y) exit
        end do       
        if ( ite == (MAX_ITE+1)) then
            exito = .false.
        else
            exito = .true.
        end if
    end if
    close(1)
end subroutine biseccion

subroutine secante(f,aext,bext,tol_x,tol_y,p,fp,ite,max_ite,error_r_x,error_a_y,exito)
    implicit none
    INTEGER(KIND=int32), PARAMETER ::wp32=il
    INTEGER(KIND=int32), PARAMETER ::wpr=rd
    real(KIND=wpr)                   ::f
    REAL(KIND=wpr), intent(in)       :: aext
    REAL(KIND=wpr), intent(in)       :: bext
    REAL(KIND=wpr), intent(in)       :: tol_x
    REAL(KIND=wpr), intent(in)       :: tol_y
    REAL(KIND=wpr), intent(out)      :: p 
    REAL(KIND=wpr), intent(out)      :: fp 
    REAL(KIND=wpr), intent(out)      :: error_r_x
    REAL(KIND=wpr), intent(out)      :: error_a_y

    INTEGER(KIND=wp32), intent(in)    ::MAX_ITE
    INTEGER(KIND=wp32), intent(out)   ::ite

    LOGICAL, intent(out)             ::exito
    
    REAL(KIND=wpr)                   ::fa
    REAL(KIND=wpr)                   ::fb
    REAL(KIND=wpr)                   ::a
    REAL(KIND=wpr)                   ::b
    REAL(KIND=wpr)                   ::p1

    a=aext
    b=bext
    fa= f(a)
    fb= f(b)
    p=b
    p1 = a - (fa * (b - a)) / (fb - fa)
    if(fa*fb > 0) then
        exito = .false.
    else
        do ite=1,MAX_ITE
            p1 = a - (fa * (b - a)) / (fb - fa)
            fp= f(p)

            a=b
            b=p1
            fa = f(a)
            fb= f(b)
            error_r_x = ABS(p1-p)/ABS(p)
            error_a_y = ABS(fp)
            
            write(*,66) ite,p,fp,error_r_x,error_a_y, a, b
66          FORMAT(I4, 6(2x,E18.12))

            p = p1
            if( error_r_x < tol_x  .and.  error_a_y<tol_y) exit
        end do       
        if ( ite == (MAX_ITE+1)) then
            exito = .false.
        else
            exito = .true.
        end if
    end if
end subroutine secante


subroutine regula_falsi(f,aext,bext,tol_x,tol_y,p,fp,ite,max_ite,error_r_x,error_a_y,exito)
    implicit none
    INTEGER(KIND=int32), PARAMETER ::wp32=il
    INTEGER(KIND=int32), PARAMETER ::wpr=rd
    real(KIND=wpr)                   ::f
    REAL(KIND=wpr), intent(in)       :: aext
    REAL(KIND=wpr), intent(in)       :: bext
    REAL(KIND=wpr), intent(in)       :: tol_x
    REAL(KIND=wpr), intent(in)       :: tol_y
    REAL(KIND=wpr), intent(out)      :: p 
    REAL(KIND=wpr), intent(out)      :: fp 
    REAL(KIND=wpr), intent(out)      :: error_r_x
    REAL(KIND=wpr), intent(out)      :: error_a_y

    INTEGER(KIND=wp32), intent(in)    ::MAX_ITE
    INTEGER(KIND=wp32), intent(out)   ::ite

    LOGICAL, intent(out)             ::exito
    
    REAL(KIND=wpr)                   ::fa
    REAL(KIND=wpr)                   ::fb
    REAL(KIND=wpr)                   ::a
    REAL(KIND=wpr)                   ::b
    REAL(KIND=wpr)                   ::p1

    a=aext
    b=bext
    fa= f(a)
    fb= f(b)

    if(fa*fb > 0) then
        exito = .false.
    else
        do ite=1,MAX_ITE
            p = p1
            p1 = a - (fa * (b - a)) / (fb - fa)
            fp= f(p)

            if (fa*fp < 0) then
                b = p
                fb=f(b)
            else
                a = p
                fa=f(a)
            end if
            error_r_x = ABS(p1-p)/ABS(p)
            error_a_y = ABS(fp)
            
            write(*,66) ite,p,fp,error_r_x,error_a_y, a, b
66          FORMAT(I4, 6(2x,E18.12))

            if( error_r_x < tol_x  .and.  error_a_y<tol_y) exit
        end do       
        if ( ite == (MAX_ITE+1)) then
            exito = .false.
        else
            exito = .true.
        end if
    end if
end subroutine regula_falsi



subroutine newton(f,df,pext,tol_Y,tol_X,p,fp,ite,max_ite,error_r_x,error_a_y,exito)
implicit none

INTEGER (KIND=int32), PARAMETER                 :: wp32 = il
INTEGER(KIND =int32), PARAMETER                 :: wpr = rd

REAL(KIND=wpr)                                   ::f
REAL(KIND=wpr)                                   ::df

REAL(KIND=wpr),intent(in)                        ::pext
REAL(KIND=wpr),intent(in)                        ::tol_x
REAL(KIND=wpr),intent(in)                        ::tol_y
INTEGER(KIND=wp32),intent(in)                     ::max_ite

REAL(KIND=wpr),intent(out)                       ::p
REAL(KIND=wpr),intent(out)                       ::fp
REAL(KIND=wpr)                                   ::dfp
INTEGER(KIND=wp32),intent(out)                    ::ite
REAL(KIND=wpr),intent(out)                       ::error_r_x
REAL(KIND=wpr),intent(out)                       ::error_a_y
LOGICAL, intent(out)                             ::exito

p= pext
fp = f(p)
dfp = df(p)
DO ite=1,max_ite

    p = p - fp / dfp

    fp = f(p)
    dfp = df(p)

    error_a_y  = ABS(fp)
    error_r_x  = ABS( p - (p - fp/dfp) ) / ABS( p)
    write(*,66) ite, p, fp, error_a_y, error_r_x
66  FORMAT(I4, 4(2x,E18.12))
    IF ( error_a_y < tol_y  .AND. error_r_x < tol_x) exit
    if ( error_r_x < tol_x  .and. error_a_y < tol_y) exit

END DO

IF(ite > max_ite) THEN 
    exito = .false.
ELSE 
    exito = .true.
END IF


end subroutine newton


subroutine newton_modificado(f,df,ddf,pext,tol_Y,tol_X,p,fp,ite,max_ite,error_r_x,error_a_y,exito)
implicit none

INTEGER (KIND=int32), PARAMETER                 :: wp32 = il
INTEGER(KIND =int32), PARAMETER                 :: wpr = rd

REAL(KIND=wpr)                                   ::f
REAL(KIND=wpr)                                   ::df
REAL(KIND=wpr)                                   ::ddf

REAL(KIND=wpr),intent(in)                        ::pext
REAL(KIND=wpr),intent(in)                        ::tol_x
REAL(KIND=wpr),intent(in)                        ::tol_y
INTEGER(KIND=wp32),intent(in)                     ::max_ite

REAL(KIND=wpr),intent(out)                       ::p
REAL(KIND=wpr)                                   ::p1
REAL(KIND=wpr),intent(out)                       ::fp
REAL(KIND=wpr)                                   ::dfp
REAL(KIND=wpr)                                   ::ddfp
INTEGER(KIND=wp32),intent(out)                    ::ite
REAL(KIND=wpr),intent(out)                       ::error_r_x
REAL(KIND=wpr),intent(out)                       ::error_a_y
LOGICAL, intent(out)                             ::exito

p= pext
fp = f(p)
dfp = df(p)
ddfp = ddf(p)
DO ite=1,max_ite

    p1 = p - (fp * dfp**2)/(dfp**3 - fp*dfp*ddfp)

    fp = f(p1)
    dfp = df(p1)
    ddfp = ddf(p1)

    error_a_y  = ABS(fp)
    error_r_x  = ABS( p1 - (p1 - fp/dfp) ) / ABS( p1)
    write(*,66) ite, p1, fp, error_a_y, error_r_x
66  FORMAT(I4, 4(2x,E18.12))
    IF ( error_a_y < tol_y  .AND. error_r_x < tol_x) exit
    if ( error_r_x < tol_x  .and. error_a_y < tol_y) exit

    p = p1
END DO

IF(ite > max_ite) THEN 
    exito = .false.
ELSE 
    exito = .true.
END IF


end subroutine newton_modificado

end module metodos