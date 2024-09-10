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
