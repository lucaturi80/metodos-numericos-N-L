module metodos

use isoprecisiones
use funciones

implicit none

contains

!********************************************************
!METODO DE BISECCION
!********************************************************

subroutine biseccion(f,a,b,tol_x,tol_y,raiz,max_ite,Nite,unidad)

!*************************************************************************
! f: funcion a encontrar la raiz
! a y b: limites inferiores y superiores del interbalo en x
! tol_x y tol_y: el error maximo permitido en x e y
! raiz: resultado final del programa
! max_ite: numero maximo de iteraciones permitidas
! Nite: numero final de iteraciones que le tomo
! unidad: archivo de datos donde se van a inprimir los resulrtados
!************************************************************************

!defino variables de la subrutina
real(kind=dp),intent(in)   :: a 
real(kind=dp),intent(in)   :: b
real(kind=dp),intent(in)   :: tol_x
real(kind=dp),intent(in)   :: tol_y
real(kind=dp),intent(out)  :: raiz
real(kind=dp),intent(in)   :: max_ite
integer(kind=ed),intent(out)  :: Nite
integer(kind=ed),intent(in)   :: unidad
real(kind=dp)   :: f

!defino variables auxiliares*********************************************************
real(kind=dp) :: fa, fb, p, fp, errorx, errory
real(kind=dp) :: ai, bi

! ai y bi los extremos del interbalo en cada iteracion
! fa y fb los valores de los extremos del interbalo evaluados en f en cada iteracion
! p y fp son el punto medio del intervalo y este ebaluado en f

ai = a
bi = b

!programa******************************************************************

fa = f(a)            !evaluo la fucion en a y en b 
fb = f(b)            ! y seteo las iteraciones en 0
Nite = 0_ed          !

! si la fa y fb tienen el mismo signo, digo que ingrese denuevo
if(fa * fb > 0.0_dp )then
  print*, "la funcion tiene el mismo signo en a y en b, ingrese de nuevo el interbalo"
  stop
end if

write(unidad,*) "n° ite","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"

!hago el primer paso del metodo
p = a + (b - a) / 2.0_dp    !le resto a el extremo inferior del intervalo la mitad de la longitud del inervalo
fp = f(p)
!print*, a,b
!print*, p, fp

!mira el signo y redefine los extremos de los intebalos
do
  if(fa * fp < 0.0_dp)then 
     bi = p      !si fa y fp tienen signo distinto la raiz esta entre a y p
     fb = fp     !entonces cambiamos el extremo superior para que sea igual a p
  else 
    ai = p       !si fa y fp tienen signo igual la raiz esta entre b y p
    fa = fp      !entonces cambiamos el extremo inferior para que sea igual a p
  end if
    
  !hace otro paso del metodo y evalua la funcion el el punto para chequiar el error en y
  p = ai + (bi - ai) / 2.0_dp
  fp = f(p)
  Nite = Nite + 1_ed
  
  !calculo el error relativo en x y el absoluto en y
  errorx = abs(bi-ai) / abs(ai + bi)
  errory = abs(fp)
  
  if((errorx < tol_x).and.(errory < tol_y)) exit
  if(Nite > max_ite) exit
  
  write(unidad,66) Nite,p,fp,errorx,errory   !p es la xn, fp es la funcion evaluada en xn
 
  end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p

write(unidad,*)"ite final","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"
write(unidad,66) Nite,p,fp,errorx,errory   !p es la xn, fp es la funcion evaluada en xn

write(*,*)"ite final","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"
write(*,66) Nite,p,fp,errorx,errory   !p es la xn, fp es la funcion evaluada en xn

66 FORMAT(I4, 6(x,E18.9))

end subroutine biseccion


!********************************************************
!METODO DE LA SECANTE
!********************************************************

subroutine secante(f,a,b,tol_x,tol_y,raiz,max_ite,Nite,unidad)

!*************************************************************************
! f: funcion a encontrar la raiz
! a y b: limites inferiores y superiores del interbalo en x
! tol_x y tol_y: el error maximo permitido en x e y
! raiz: resultado final del programa
! max_ite: numero maximo de iteraciones permitidas
! Nite: numero final de iteraciones que le tomo
! unidad: archivo de datos donde se van a inprimir los resulrtados
!************************************************************************

!defino las variables DUMY

real(kind=dp),intent(in)   :: a 
real(kind=dp),intent(in)   :: b
real(kind=dp),intent(in)   :: tol_x
real(kind=dp),intent(in)   :: tol_y
real(kind=dp),intent(out)  :: raiz
real(kind=dp),intent(in)   :: max_ite
integer(kind=ed),intent(out)  :: Nite
integer(kind=ed),intent(in)   :: unidad
real(kind=dp)   :: f

!defino las variables del programa
real(kind=dp) :: p0, p1, f1, f0, p2, f2, errorx, errory

!p1 y p0 son los valores de los extemos de los interbalos en cada iteracion
!f1 y f0 son los extremos de los intervalos evaluados en f
!p2 es el punto en el que la secante corta el 0
!fp es p ebaluado en f

!programa

p1 = b         !
p0 = a         !
f1 = f(b)      ! inicializo las variables del programa
f0 = f(a)      !
Nite = 0_ed    !

write(unidad,*) "n° ite","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"

p2 = p1 - f1 * ((p1 - p0) / (f1 - f0))
f2 = f(p2)

!print*, p2,f2

do
 p0 = p1
 p1 = p2
 
 f1 = f(p1)
 f0 = f(p0)
 
 
 p2 = p1 - f1 * ((p1 - p0) / (f1 - f0))
 f2 = f(p2)

 errorx = abs(p2 - p1) / abs(p2)
 errory = abs(f2)

 Nite = Nite + 1_ed

 if((errorx < tol_x).and.(errory < tol_y)) exit
 if(Nite > max_ite) exit

 write(unidad,66) Nite,p2,f2,errorx,errory   !p2 es la xn, f2 es la funcion evaluada en xn

end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p2

write(unidad,*)"ite final","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"
write(unidad,66) Nite,p2,f2,errorx,errory   !p2 es la xn, f2 es la funcion evaluada en xn

write(*,*)"ite final","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"
write(*,66) Nite,p2,f2,errorx,errory   !p2 es la xn, f2 es la funcion evaluada en xn

66 FORMAT(I4, 6(x,E18.9))

end subroutine secante


!********************************************************
!METODO DE NEWTON
!********************************************************

subroutine newton(f,df,a,tol_x,tol_y,raiz,max_ite,Nite,unidad)

!*************************************************************************
! f: funcion a encontrar la raiz
! df: derivada de la funcion
! a : punto por el que empieza a funcionar
! tol_x y tol_y: el error maximo permitido en x e y
! raiz: resultado final del programa
! max_ite: numero maximo de iteraciones permitidas
! Nite: numero final de iteraciones que le tomo
! unidad: archivo de datos donde se van a inprimir los resulrtados
!************************************************************************

!defino las variables DUMY

real(kind=dp),intent(in)   :: a 
real(kind=dp),intent(in)   :: tol_x
real(kind=dp),intent(in)   :: tol_y
real(kind=dp),intent(out)  :: raiz
real(kind=dp),intent(in)   :: max_ite
integer(kind=ed),intent(out)  :: Nite
integer(kind=ed),intent(in)   :: unidad
real(kind=dp)   :: f, df

!defino variables auxiliares
real(kind=dp) :: fa, dfa, p, fp, errorx, errory
real(kind=dp) :: ai

! ai los extremos del interbalo en cada iteracion
! fa y fb los valores de los extremos del interbalo evaluados en f en cada iteracion
! p y fp son el punto medio del intervalo y este ebaluado en f

!inicializo las variables auxiliares
Nite = 0_ed
ai = a 
fa = f(a)
dfa = df(a)

write(unidad,*) "n° ite","     ","xn","         ","f en xn","         ","error rel x","             ","error abs y"


do
 p = ai - fa / dfa
 fp = f(p)
 
 errorx = abs(p - ai) / abs(p)
 errory = abs(fp)

 Nite = Nite + 1_ed

 if((errorx < tol_x).and.(errory < tol_y)) exit
 if(Nite > max_ite) exit

write(unidad,66) Nite,p,fa,errorx,errory

ai = p
fa = fp
dfa = df(p)

end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p

write(unidad,*)"ite final","     ","xn (raiz)","         ","f en xn","         ","error rel x","             ","error abs y"
write(unidad,66) Nite,p,fa,errorx,errory   !p es la xn, fa es la funcion evaluada en xn

write(*,*)"ite final","     ","xn (raiz)","         ","f en xn","         ","error rel x","             ","error abs y"
write(*,66) Nite,p,fa,errorx,errory   !p es la xn, fa es la funcion evaluada en xn

66 FORMAT(I4, 6(x,E18.9))

end subroutine newton


end module metodos

