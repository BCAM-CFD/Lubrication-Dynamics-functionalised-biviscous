!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!   BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
!
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!   at UNED, in Madrid, Spain.
!
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!   Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

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
