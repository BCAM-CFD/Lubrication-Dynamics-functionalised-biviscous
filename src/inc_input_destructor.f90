!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------
SUBROUTINE input_destructor(this)
  !----------------------------------------
  ! Destructor of the input object
  !----------------------------------------
  IMPLICIT NONE
  TYPE(input_type), INTENT(inout) :: this

  IF (ALLOCATED(this%L)) THEN
     DEALLOCATE(this%L)
  ENDIF

END SUBROUTINE input_destructor
