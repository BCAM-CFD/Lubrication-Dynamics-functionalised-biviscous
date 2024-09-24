!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!*********************************************
!  CLASS READ_INPUT FILE
!*********************************************

!---------------- read_input_constructor ------------------
  
  !--------------------------------------------------
  SUBROUTINE read_input_constructor(this, file, variable_name)
    !--------------------------------------------------
    ! Constructor of the read_input class
    !--------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout)          :: this
    CHARACTER(LEN=MAX_CHAR), OPTIONAL, INTENT(in) :: file
    CHARACTER(LEN=MAX_CHAR), OPTIONAL, INTENT(in) :: variable_name

    IF (present(file)) THEN
       this%file = file
    ELSE
       this%file = 'input'
    ENDIF
    this%variable_name = variable_name

  END SUBROUTINE read_input_constructor

!-------------END read_input_constructor ------------------
