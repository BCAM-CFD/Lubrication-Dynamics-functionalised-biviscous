!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------
  SUBROUTINE file_destructor(this)
    !------------------------------------
    ! Destructor of class_file.
    !------------------------------------
    use class_files_utilities
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout)       :: this
    LOGICAL :: opened

    !-- We check if its unit is still opened or not --
    INQUIRE(unit=this%unit, opened = opened) 
    IF (opened) THEN
       CLOSE(this%unit)
    ENDIF
    
  END SUBROUTINE file_destructor
