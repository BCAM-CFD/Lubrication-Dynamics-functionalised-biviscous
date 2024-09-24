!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------
SUBROUTINE initialize_comp_time(this)
  !------------------------------------------
  ! Initializing conputational times.
  !------------------------------------------
  IMPLICIT NONE
  TYPE(comp_time_type) :: this

  this%total     = 0.0_Pr
  this%neigh     = 0.0_Pr
  this%semi_impl = 0.0_Pr
  this%VV        = 0.0_Pr

END SUBROUTINE initialize_comp_time
