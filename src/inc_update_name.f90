!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-------------------------------------------------
  SUBROUTINE update_name(this, step)
!-------------------------------------------------
    ! We update the name of the file with the 
    ! corresponding time step.
    !---------------------------------------------
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout) :: this
    INTEGER, INTENT(in)            :: step
    CHARACTER(LEN=MAX_CHAR) :: termination

    WRITE(termination,'(I10.10,A)')  step,'.dat'
    this%name = trim(this%base_name)//trim(termination)

  END SUBROUTINE update_name
