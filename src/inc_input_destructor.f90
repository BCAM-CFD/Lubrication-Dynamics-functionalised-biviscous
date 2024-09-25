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

!---------------------------------------
SUBROUTINE input_destructor(this)
  !----------------------------------------
  ! Destructor of the input object
  !----------------------------------------
  IMPLICIT NONE
  TYPE(input_type), INTENT(inout) :: this

  IF (ALLOCATED(this%L)) THEN
     DEALLOCATE(this%L)
  ENDIF

END SUBROUTINE input_destructor
