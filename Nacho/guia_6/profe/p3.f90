program integracion
!
! cálculo de integral definida usando
! métodos de trapecio y regla de Simpson .
!
! Notar que en este programa : nn = número de evaluaciones
!
!   de la función



use precsn
use funciones
use inte_trape_simp

implicit none

integer                               :: p , p_max , p_max3 , nn , nn2 , j , k , ntot
real ( kind = wp )                    :: x_ini , x_fin , tol , h , x , integr ,err_1 , err_2 , int_exacta , q

integer                               :: t1 , t2 , clock_rate , clock_max
real ( kind = wp )                    :: start_time , end_time

! lectura de parametros -------------------------------------------------------------------
namelist /parametroscmp/                       &
                 tol     ! tolerancia permitida


namelist /parametrosinic/                  &
                x_ini , x_fin , nn ! rango de integracion [ x_ini , x_fin ] , numero total de puntos

 open  (unit = 100, file = "./datosent/paramscmp.in" , status = "old" )
 read  (unit = 100, nml = parametroscmp )
 close (unit = 100)

 open  (unit = 11, file = "./datosent/paramsinic-prob3.in" , status ="old" )
 read  (unit = 11, nml = parametrosinic )
 close (unit = 11)

 ! ----------------------
 open ( unit = 101 , file = "./datossal/salidas-prob3.datos" , status = "unknown")

write (* ,*)     "x_ini =" , x_ini
write (* ,*)     "x_fin =" , x_fin

write (* ,*)     "nn =" ,nn
write (* ,*)     " h =" ,( x_fin - x_ini ) /( 1.0_wp *( nn-1))
write (101 ,*)   " x_ini = " , x_ini
write (101 ,*)   " x_fin ="  , x_fin

write (101 ,*) " nn = " , nn
write (101 ,*) " h =" ,( x_fin - x_ini ) /( 1.0_wp *( nn-1))

write (* ,*) ' tol = ',tol , ' que no se usa en este problema, pero si en el siguiente '
write (* ,*) " "
write (101 ,*) " "

int_exacta = -( funcion ( x_fin ) - funcion ( x_ini ))

write(*,*) "integral exacta de exp(-x) = ", int_exacta

!!*****************************************************************************

call cpu_time ( start_time )
call puntomedio (integr, x_ini, x_fin, nn)

! ----------------------------------------------------
! ----------------------------------------------------
write (* , '( a33 , e42.35 , a7 , e42.35 , a7 , i7 ) ') " integral regla de puntomedio = " , integr , &
          " h = " ,( x_fin - x_ini ) /( 1.0 _wp *( nn-1) ) ," nn = " ,nn

write (101 , '( a33 , e42.35 , a7 , e42.35 , a7 , i7 ) ') " integral regla de puntomedio = " , integr , &
         " h =  " ,( x_fin - x_ini ) /( 1.0 _wp *( nn-1) ) ," nn = " ,nn

 err_1 = int_exacta - integr
 nn2 = 2* nn -1
 call puntomedio ( integr , x_ini , x_fin , nn2 )
 call cpu_time ( end_time )
 write ( *, * ) ' medidas del tiempo de ejecución: '
 write (101 , * ) ' medidas del tiempo de ejecución: '
 write ( *, * ) ' Elapsed CPU time = ', start_time - end_time
 write (101 , * ) ' Elapsed CPU time = ', start_time - end_time

 write ( *, * )
 write (101 , * )

 ! ----------------------------------------------------

 write (* , '( a33 ,e42.35 , a7 ,e42.35 , a7 , i7 ) ') " integral regla de puntomedio = " , integr , &
                    " h = " ,( x_fin - x_ini ) /( 1.0_wp *( nn2-1) ) ," nn2 = " , nn2

write (101, '( a33 ,e42.35 , a7 ,e42.35 , a7 , i7 ) ') " integral regla de puntomedio = " , integr , &
                    " h = " ,( x_fin - x_ini ) /( 1.0_wp *( nn2-1) ) ," nn2 = " , nn2


err_2 = int_exacta - integr

q = err_1 / err_2

write(* ,*) " "
write (101 ,*) " "
write(* ,*) "Q_puntomedio = " ,q
write(101 ,*) "Q_puntomedio = " ,q

write (* ,*) " "
write (101 ,*) " "








!***************************************************************************
!---------------------------------------------------------------------
! medida del tiempo de ejecucion --------------------

call cpu_time ( start_time )
call trapecio (integr, x_ini, x_fin, nn)

! ----------------------------------------------------
! ----------------------------------------------------
write (* , '( a33 , e42.35 , a7 , e42.35 , a7 , i7 ) ') " integral regla de trapecio = " , integr , &
          " h = " ,( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " , nn

write (101 , '( a33 , e42.35 , a7 , e42.35 , a7 , i7 ) ') " integral regla de trapecio = " , integr , &
         " h =  " ,( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " , nn

 err_1 = int_exacta - integr
 nn2 = 2* nn -1
 call trapecio ( integr , x_ini , x_fin , nn2 )
 call cpu_time ( end_time )
 write ( *, * ) ' medidas del tiempo de ejecución: '
 write (101 , * ) ' medidas del tiempo de ejecución: '
 write ( *, * ) ' Elapsed CPU time = ', start_time - end_time
 write (101 , * ) ' Elapsed CPU time = ', start_time - end_time

 write ( *, * )
 write (101 , * )

 ! ----------------------------------------------------

 write (* , '( a33 ,e42.35 , a7 ,e42.35 , a7 , i7 ) ') " integral regla de trapecio = " , integr , &
                    " h = " ,( x_fin - x_ini ) /( 1.0_wp *( nn2-1) ) ," nn2 = " , nn2

write (101, '( a33 ,e42.35 , a7 ,e42.35 , a7 , i7 ) ') " integral regla de trapecio = " , integr , &
                    " h = " ,( x_fin - x_ini ) /( 1.0_wp *( nn2-1) ) ," nn2 = " , nn2


err_2 = int_exacta - integr

q = err_1 / err_2

write(* ,*) " "
write (101 ,*) " "
write(* ,*) "Q_trapecio = " ,q
write(101 ,*) "Q_trapecio = " ,q

write (* ,*) " "
write (101 ,*) " "

! ---------------------------------------------------------------------
! ahora con Simpson
! medida del tiempo de ejecucion --------------------

call cpu_time ( start_time )
call simpson ( integr , x_ini , x_fin , nn )

! --------------------------------------------------------------------------


write (* , '( a33 ,e42.35 ,a7 ,e42.35 ,a7 ,i7)') " integral de Simpson = ", integr, &
             "  h =  ",( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " ,nn

write (101, '( a33 ,e42.35 ,a7 ,e42.35 ,a7 ,i7)') " integral de Simpson = ", integr, &
                          "  h =  ",( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " ,nn

err_1 = int_exacta - integr
nn2 = 2* nn -1
call simpson ( integr , x_ini , x_fin , nn2 )

call cpu_time ( end_time )
write ( *, * ) ' medidas del tiempo de ejecución : '
write (101 , * ) ' medidas del tiempo de ejecución: '
write ( *, * ) ' Elapsed CPU time = ', start_time - end_time
write (101 , * ) ' Elapsed CPU time = ', start_time - end_time
write (* ,*) " "
write (101 ,*) " "



write (* , '( a33 ,e42.35 ,a7 ,e42.35 ,a7 ,i7)') " integral de Simpson = ", integr, &
             "  h =  ",( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " ,nn

write (101, '( a33 ,E42.35 ,a7 ,e42.35 ,a7 ,i7)') " integral de Simpson = ", integr, &
                          "  h =  ",( x_fin - x_ini ) /( 1.0 _wp *( nn -1) ) ," nn = " ,nn


err_2 = int_exacta - integr
q = err_1 /err_2

write (* ,*) " "
write (101 ,*) " "

write (* ,*)   " Q_simpson = " , q
write (101 ,*) " Q_simpson = " , q


write (* ,*) " "
write (101 ,*) " "

write (* ,'( a33 ,E42.35 )')   " integral exacta = " , int_exacta
write (101 ,*) " integral exacta = " , int_exacta

close ( unit = 101)

end program integracion
