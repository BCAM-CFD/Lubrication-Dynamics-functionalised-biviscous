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

!------------------------------------------------------
SUBROUTINE read_random(this, input, error_out)
!------------------------------------------------------
! Quantities of class random are read from input.
!-------------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(random_type), INTENT(inout)    :: this
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input
  INTEGER, INTENT(out)                :: error_out
  CHARACTER(LEN=MAX_CHAR) :: name

  !--------------  fixed_seed ----------------------
  name = 'fixed_seed'
  CALL read_variable(input, name, this%fixed_seed, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--------------  seed ----------------------
  name = 'seed'
  CALL read_variable(input, name, this%seed, error_out)
  IF (error_out .NE. 0) THEN
     error_out = 1
     GOTO 1000 !-- End of subroutine --
  ENDIF

1000 CONTINUE

END SUBROUTINE read_random
