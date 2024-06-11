module integrales
!-----------------------------------------
!precision
!-----------------------------------------
use isoprec
INTEGER(KIND=il),parameter ::ni=il, nr=rd
!-----------------------------------------

!METODOS DE INTEGRACION 
contains
!PUNTO MEDIO
SUBROUTINE punto_medio(n,h,fx,integral)
!como para este metodo nos pide que tiene que evaluada la funcion en todos los puntos xi
!vamos a inventar valores para x interedias entre xi y xi+1, estos valores no serán usados para el calculo(la regla del punto medio los omite)
!pero nos permitirá partir la h en 2 y de esta forma evaluar todos los puntos f(xi+h) que es lo que nos pide la consigna.
!(ver imagen "PUNTO_MEDIO.png")
!los rectangulos de los extremos seran sumados aparte
!-----------------------------------------
!declaracion de variables
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                     ::n
REAL(KIND=nr),intent(in)                        ::h
REAL(KIND=nr),intent(in), dimension(-1:n+1)     ::fx 
INTEGER(KIND=ni)                                ::n2
REAL(KIND=nr), dimension(-1:n*2+3)              ::fx2

!fx vector con valores de la funcion tal que 
!fx(-1)=f(a),fx(0)=f(a+h),..., fx(i)= f(a+(i+1)h),..,fx(n+1)=f(b)
!fx2 es un vector tal que  fx2(-1) = f(a+h/2), fx2(0) = f(a+h)=fx(0)
!fx2(2*i) = fx(i), fx2(2i+1) = valores inventados, fx2(n2 + 1) = f(b-h/2) lo vamos a inventar
INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral

n2 = n*2+2
!-----------------------------------------

!-----------------------------------------
!invencion de puntos intermedios
!-----------------------------------------
fx2(-1) = 0 
do i = 0, n+1
    fx2(2*i)=fx(i)
    fx2(2*i+1)=0
end do

!-----------------------------------------


!-----------------------------------------
!proceso
!-----------------------------------------

!trato especial para extremos
integral = 0 !0.5_nr *(fx(-1) + fx(n+1)) !esos rectangulitos son mas chiquitos por eso miden la mitad
do i=0,n2,2
    integral = integral + fx2(i)
end do
integral = integral * h !no hace falta multiplicarlo por 2 puesto que estos rectangulos mas finos miden la mitad, o sea solo h
!-----------------------------------------
END SUBROUTINE


!TRAPECIO

SUBROUTINE trapecio(n,h,fx,integral)
!-----------------------------------------
!declaracion de variables
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                        ::n
REAL(KIND=nr),intent(in)                           ::h
REAL(KIND=nr),intent(in), dimension(0:n)           ::fx 
!vector con valores de la funcion tal que fx(-1)=f(a),fx(0)=f(a+h),..., fx(i)= f(a+(i+1)h),..,fx(n+1)=f(b)

INTEGER(KIND=ni)                                   ::i
REAL(kind=nr),  intent(out)                        ::integral

!-----------------------------------------

!-----------------------------------------
!proceso
!-----------------------------------------
integral = fx(0) + fx(n)
do i=1,n-1
    integral = integral + 2 * fx(i) 
end do
integral = integral * (h / 2)
!-----------------------------------------
END SUBROUTINE



!SIMPSON
SUBROUTINE SIMPSON(n,h,fx,integral)
!-----------------------------------------
!declaracion de variables
!-----------------------------------------
implicit none
INTEGER(KIND=ni),intent(in)                     ::n
REAL(KIND=nr),intent(in)                        ::h
REAL(KIND=nr),intent(in), dimension(0:n)        ::fx 
!vector con valores de la funcion tal que fx(0)=f(a),fx(1)=f(a+h),..., fx(i)= f(a+ih),..,fx(n)=f(b)

INTEGER(KIND=ni)                                ::i
REAL(kind=nr),  intent(out)                     ::integral

!-----------------------------------------

!-----------------------------------------
!proceso
!-----------------------------------------
write(*,*) h,"/3 * (", fx(0),"+",fx(n)
integral = (fx(0) + fx(n)) !trato esp para extremos
do i=1,n-1
    ! a los pares los mult por 2 y a los imp por 4
    if(mod(i,2)==0) THEN
        integral = integral + 2*(fx(i))
        write(*,*) "+ 2 * ", fx(i)
    ElSE
        integral = integral + 4*(fx(i))
        write(*,*) "+ 4 * ", fx(i)
    END IF
end do
write(*,*) ")"
integral = integral*h/3
!-----------------------------------------
END SUBROUTINE




end module