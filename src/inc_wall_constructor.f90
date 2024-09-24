!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-----------------------------------------
SUBROUTINE wall_constructor(this, dim, error_out)
!-----------------------------------------
  ! Constructor of class wall
  !---------------------------------------
  IMPLICIT NONE
  TYPE(wall_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: dim
  INTEGER, INTENT(out)           :: error_out
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_wall_constructor.f90'  

  IF (dim .NE. 3) THEN
     error_out = 1
     CALL error_header(file_name)     
     WRITE(*,*) '*** Particle constructor error: the code has been only &
          checked in 3D. ***'
     GOTO 1000 !-- End of subroutine ---
  ENDIF
     
  ALLOCATE(this%vel_top(dim))
  ALLOCATE(this%force_top(dim))
  ALLOCATE(this%lub_force_top(dim))
  ALLOCATE(this%rep_force_top(dim))
  ALLOCATE(this%vel_bottom(dim))
  ALLOCATE(this%force_bottom(dim))
  ALLOCATE(this%lub_force_bottom(dim))
  ALLOCATE(this%rep_force_bottom(dim))
  
1000 CONTINUE

END SUBROUTINE wall_constructor
