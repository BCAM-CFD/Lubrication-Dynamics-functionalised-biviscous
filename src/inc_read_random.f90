!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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
