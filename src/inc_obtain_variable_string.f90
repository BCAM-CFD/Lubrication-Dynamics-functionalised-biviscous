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

!---------------------- obtain_variable_string ------------------------------

!---------------------------------------------------
  SUBROUTINE obtain_variable_string(this)
    !---------------------------------------------------
    ! The line starting by the name of the variable has been stored.
    ! This subroutine finds and stores the variable (in string format)
    ! written in such a line
    !---------------------------------------------------
    IMPLICIT NONE
    TYPE(read_input_type), INTENT(inout) :: this
    INTEGER :: length, total_length
    CHARACTER(LEN = MAX_CHAR) :: line_aux

    length = this%length_name_variable
    total_length = len(this%line)

    line_aux = this%line(length + 1:total_length) !-- name of variable is deleted 
    line_aux = adjustl(line_aux)

    line_aux = line_aux(2:total_length) !-- = symbol is deleted --
    line_aux = adjustl(line_aux)
    
    this%variable_string = line_aux

  END SUBROUTINE obtain_variable_string

!------------------ END obtain_variable_string ------------------------------
