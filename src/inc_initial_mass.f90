!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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
