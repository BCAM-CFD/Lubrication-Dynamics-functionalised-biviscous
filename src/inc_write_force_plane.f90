!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_force_plane(this, step)
  !---------------------------------------------
  ! Data about the calculated forces in planes is written out
  ! in the file "plane.dat".
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: step
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !--- The formatting is calculated ---
  IF (this%dim == 2) THEN
     formatting = '(I10, 6E20.10)'
  ELSE
     formatting = '(I10, 9E20.10)'
  ENDIF

  WRITE(this%output%plane%unit, formatting)   &    !2D    !3D
       step,                                  &    !1     !1
       this%force_plane_x,                    &    !2,3   !2,3,4
       this%force_plane_y,                    &    !4,5   !5,6,7
       this%force_plane_z                          !6,7   !8,9,10

END SUBROUTINE write_force_plane
