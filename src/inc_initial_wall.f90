!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!    BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!    at UNED, in Madrid, Spain.
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!    Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

!-------------------------------------------------
SUBROUTINE initial_wall(this, Vwall, error_out)
!-------------------------------------------------
  ! Subroutine to initialize the wall object.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(wall_type), INTENT(inout)     :: this
  REAL(Pr), DIMENSION(:), INTENT(in) :: Vwall
  INTEGER, INTENT(out)               :: error_out
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  
  error_out = 0

  file_name = 'inc_initial_wall.f90'  
  
  IF (SIZE(Vwall) .NE. SIZE(this%vel_top)) THEN
     error_out = 1
     CALL error_header(file_name)     
     WRITE(*,*) '*** initial wall error: the size of Vwall is incorrect. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  this%vel_top(:)    = Vwall(:)
  this%vel_bottom(:) = -Vwall(:)
  
1000 CONTINUE

END SUBROUTINE initial_wall
