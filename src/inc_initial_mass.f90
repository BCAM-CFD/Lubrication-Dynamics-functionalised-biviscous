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

!------------------------------------------------
SUBROUTINE initial_mass(this, m)
  !------------------------------------------------
  ! The mass is assigned to the particles.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)             :: m

  this%part(:)%mass = m
  
END SUBROUTINE initial_mass
