!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

module class_comp_time
  !---------------------------------
  ! Class related to computational times
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE comp_time_type
     
     REAL(Pr) :: total     !-- Total time --
     REAL(Pr) :: neigh     !-- time to search for neighbours --
     REAL(Pr) :: semi_impl !-- time to do the semi-implicit method --
     REAL(Pr) :: VV        !-- time to move the simulation
                           !   (with the Velocity Verlet integrator)

  END type comp_time_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_initialize_comp_time.f90'
    include 'inc_write_comp_times.f90'
 
END module class_comp_time
