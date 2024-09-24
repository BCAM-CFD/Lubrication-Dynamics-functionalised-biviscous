!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_wall(this, step)
  !---------------------------------------------
  ! Wall info is written in the file 'walls.dat'
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: step
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !--- The formatting is calculated ---
  IF (this%dim == 2) THEN
     formatting = '(I10, 4E20.10)'
  ELSE
     formatting = '(I10, 6E20.10)'
  ENDIF

  WRITE(this%output%walls%unit, formatting)   &    !2D    !3D
       step,                                  &    !1     !1
       this%wall%force_bottom,                &    !2,3   !2,3,4
       this%wall%force_top                         !4,5   !5,6,7

END SUBROUTINE write_wall
