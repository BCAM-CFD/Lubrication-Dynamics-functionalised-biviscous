!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!----------------------------------
SUBROUTINE wall_destructor(this)
!----------------------------------
  ! Destructor of the class wall
  !--------------------------------
  IMPLICIT NONE
  TYPE(wall_type), INTENT(inout) :: this
  
  IF (ALLOCATED(this%vel_top)) THEN
     DEALLOCATE(this%vel_top)
  ENDIF

  IF (ALLOCATED(this%force_top)) THEN
     DEALLOCATE(this%force_top)
  ENDIF

  IF (ALLOCATED(this%lub_force_top)) THEN
     DEALLOCATE(this%lub_force_top)
  ENDIF

  IF (ALLOCATED(this%rep_force_top)) THEN
     DEALLOCATE(this%rep_force_top)
  ENDIF

  IF (ALLOCATED(this%vel_bottom)) THEN
     DEALLOCATE(this%vel_bottom)
  ENDIF

  IF (ALLOCATED(this%force_bottom)) THEN
     DEALLOCATE(this%force_bottom)
  ENDIF

  IF (ALLOCATED(this%lub_force_bottom)) THEN
     DEALLOCATE(this%lub_force_bottom)
  ENDIF

  IF (ALLOCATED(this%rep_force_bottom)) THEN
     DEALLOCATE(this%rep_force_bottom)
  ENDIF

END SUBROUTINE wall_destructor
