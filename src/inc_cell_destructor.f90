!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-----------------------------------------
SUBROUTINE cell_destructor(this)
!-----------------------------------------
  ! Destructor of the class cell.
  !---------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(inout) :: this

  IF (ALLOCATED(this%min_coord)) THEN
     DEALLOCATE(this%min_coord)
  ENDIF

  IF (ALLOCATED(this%max_coord)) THEN
     DEALLOCATE(this%max_coord)
  ENDIF

  IF (ALLOCATED(this%neigh_coord)) THEN
     DEALLOCATE(this%neigh_coord)
  ENDIF

  IF (ALLOCATED(this%list_part)) THEN
     DEALLOCATE(this%list_part)
  ENDIF

END SUBROUTINE cell_destructor
