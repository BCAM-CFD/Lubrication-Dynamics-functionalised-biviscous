!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!--------------------- create_file ----------------------
!---------------------------------
  SUBROUTINE file_constructor(this, name, forbidden, error_out)
    !------------------------------------
    ! Constructor of class_file.
    !------------------------------------
    use class_files_utilities
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout)       :: this
    CHARACTER(LEN=MAX_CHAR), INTENT(in)  :: name
    LOGICAL, DIMENSION(:), INTENT(inout) :: forbidden
    INTEGER, INTENT(out)                 :: error_out
    
    CALL search_unit(this%unit, error_out, forbidden)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    
    this%name      = name
    !-- base_name will be used for those files which are going to 
    !   have the step in their names --
    this%base_name = name 

1000 CONTINUE

  END SUBROUTINE file_constructor
!----------------- END create_file ----------------------
