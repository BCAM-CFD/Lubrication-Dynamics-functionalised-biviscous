!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!    BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!    at UNED, in Madrid, Spain.
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!    Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

!-----------------------------------------
SUBROUTINE cell_destructor(this)
!-----------------------------------------
  ! Destructor of the class cell.
  !---------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(inout) :: this

  IF (ALLOCATED(this%min_coord)) THEN
     DEALLOCATE(this%min_coord)
  ENDIF

  IF (ALLOCATED(this%max_coord)) THEN
     DEALLOCATE(this%max_coord)
  ENDIF

  IF (ALLOCATED(this%neigh_coord)) THEN
     DEALLOCATE(this%neigh_coord)
  ENDIF

  IF (ALLOCATED(this%list_part)) THEN
     DEALLOCATE(this%list_part)
  ENDIF

END SUBROUTINE cell_destructor
