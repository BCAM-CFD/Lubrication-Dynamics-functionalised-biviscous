!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-------------------------------------------
SUBROUTINE initial_neigh_list(this, error_out)
  !-------------------------------------------
  ! The initial neighbout list of every particle
  ! is calculated.
  !-------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out

  error_out = 0

  CALL calculate_cells_list(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  CALL calculate_neigh_list(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
  this%update_neigh = .FALSE.

1000 CONTINUE

END SUBROUTINE initial_neigh_list
