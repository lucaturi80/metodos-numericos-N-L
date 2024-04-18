module modulos

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
real(kind=rd),intent(in)   :: a 
real(kind=rd),intent(in)   :: b
real(kind=rd),intent(in)   :: tol_x
real(kind=rd),intent(in)   :: tol_y
real(kind=rd),intent(out)  :: raiz
real(kind=rd),intent(in)   :: max_ite
integer(kind=id),intent(out)  :: Nite
integer(kind=is),intent(in)   :: unidad
real(kind=rd)   :: f

!defino variables auxiliares
real(kind=rd) :: fa, fb, p, fp, errorx, errory
real(kind=rd) :: ai, bi

! ai y bi los extremos del interbalo en cada iteracion
! fa y fb los valores de los extremos del interbalo evaluados en f en cada iteracion
! p y fp son el punto medio del intervalo y este ebaluado en f

ai = a
bi = b

!programa

fa = f(a)            !evaluo la fucion en a y en b 
fb = f(b)            ! y seteo las iteraciones en 0
Nite = 0_id          !

!print*, fa, fb

! si la fa y fb tienen el mismo signo, digo que ingrese denuevo
if(fa * fb > 0.0_rd )then
  print*, "la funcion tiene el mismo signo en a y en b, ingrese de nuevo el interbalo"
  stop
end if

write(unidad,*) "n° ite","     ","f(pn)","         ","error x","             ","error y"

!hago el primer paso del metodo
p = a + (b - a) / 2.0_rd    !le resto a el extremo inferior del intervalo la mitad de la longitud del inervalo
fp = f(p)
!print*, a,b
!print*, p, fp

!mira el signo y redefine los extremos de los intebalos
do
  if(fa * fp < 0.0_rd)then 
     bi = p      !si fa y fp tienen signo distinto la raiz esta entre a y p
     fb = fp     !entonces cambiamos el extremo superior para que sea igual a p
  else 
    ai = p       !si fa y fp tienen signo igual la raiz esta entre b y p
    fa = fp      !entonces cambiamos el extremo inferior para que sea igual a p
  end if
    
  !hace otro paso del metodo y evalua la funcion el el punto para chequiar el error en y
  p = ai + (bi - ai) / 2.0_rd
  fp = f(p)
  Nite = Nite + 1_id
  
  !calculo el error relativo en x y el absoluto en y
  errorx = abs(bi-ai) / 2.0_rd
  errory = abs(fp)
  
  if((errorx < tol_x).and.(errory < tol_y)) exit
  if(Nite > max_ite) exit
  
  write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p,errorx,errory
  
  end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p

write(unidad,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p,errorx,errory

write(*,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(*,*) Nite,p,errorx,errory

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

real(kind=rd),intent(in)   :: a 
real(kind=rd),intent(in)   :: b
real(kind=rd),intent(in)   :: tol_x
real(kind=rd),intent(in)   :: tol_y
real(kind=rd),intent(out)  :: raiz
real(kind=rd),intent(in)   :: max_ite
integer(kind=id),intent(out)  :: Nite
integer(kind=is),intent(in)   :: unidad
real(kind=rd)   :: f

!defino las variables del programa
real(kind=rd) :: p0, p1, f1, f0, p2, f2, errorx, errory

!p1 y p0 son los valores de los extemos de los interbalos en cada iteracion
!f1 y f0 son los extremos de los intervalos evaluados en f
!p2 es el punto en el que la secante corta el 0
!fp es p ebaluado en f

!programa

p1 = b         !
p0 = a         !
f1 = f(b)      ! inicializo las variables del programa
f0 = f(a)      !
Nite = 0_id    !

!print*, p1,p0,f1,f0


! si la fa y fb tienen el mismo signo, digo que ingrese denuevo el intervalo
if(f1 * f0 > 0.0_rd )then
  print*, "la funcion tiene el mismo signo en a y en b, ingrese de nuevo el intervalo"
  stop
end if

write(unidad,*) "n° ite","     ","pn","         ","error x","             ","error y"

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

 errorx = abs(p2 - p1)
 errory = abs(f2)

 Nite = Nite + 1_id

 if((errorx < tol_x).and.(errory < tol_y)) exit
 if(Nite > max_ite) exit

 write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p2,errorx,errory

end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p2

write(unidad,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p2,errorx,errory

write(*,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(*,*) Nite,p2,errorx,errory

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

real(kind=rd),intent(in)   :: a 
real(kind=rd),intent(in)   :: tol_x
real(kind=rd),intent(in)   :: tol_y
real(kind=rd),intent(out)  :: raiz
real(kind=rd),intent(in)   :: max_ite
integer(kind=id),intent(out)  :: Nite
integer(kind=is),intent(in)   :: unidad
real(kind=rd)   :: f, df

!defino variables auxiliares
real(kind=rd) :: fa, dfa, p, fp, errorx, errory
real(kind=rd) :: ai

! ai los extremos del interbalo en cada iteracion
! fa y fb los valores de los extremos del interbalo evaluados en f en cada iteracion
! p y fp son el punto medio del intervalo y este ebaluado en f

!inicializo las variables auxiliares
Nite = 0_id
ai = a 
fa = f(a)
dfa = df(a)

write(unidad,*) "n° ite","     ","pn","         ","error x","             ","error y"


do
 p = ai - fa / dfa
 fp = f(p)
 
 errorx = abs(p - ai)
 errory = abs(fp)

 Nite = Nite + 1_id

 if((errorx < tol_x).and.(errory < tol_y)) exit
 if(Nite > max_ite) exit

write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p,errorx,errory

ai = p
fa = fp
dfa = df(p)

end do

if(Nite > max_ite)then
  print*, "las iteraciones maximas permitidas no alcanzaron para aproximar la raiz con el error deseado"
  stop
 end if

raiz = p

write(unidad,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(unidad,'(I4 ,4(x,E19 .12) )') Nite,p,errorx,errory

write(*,*)"ite final","     ","raiz aprox","         ","error rel x","             ","error abs y"
write(*,*) Nite,p,errorx,errory

end subroutine newton


end module modulos

