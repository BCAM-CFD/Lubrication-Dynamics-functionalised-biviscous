!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------
SUBROUTINE initial_radius(this, R)
  !------------------------------------------------
  ! The radius is assigned to every particle.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  REAL(Pr), INTENT(in)             :: R

  this%part(:)%R = R
  
END SUBROUTINE initial_radius
