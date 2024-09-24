!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

module class_random
  !---------------------------------
  ! Class of the random number generator
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE random_type

     LOGICAL :: fixed_seed !-- If the seed is fixed --
     INTEGER :: seed       !-- The seed of the random number generator --

     !-- From Gaussian random numbers used by Xin --
     INTEGER  :: iset
     REAL(Pr) :: gset
     
  END type random_type

  CONTAINS
    include 'inc_read_random.f90' !-- not used --
    include 'inc_init_random_seed.f90'
    include 'inc_random_constructor.f90'
    include 'inc_random_gaussian.f90'
    include 'inc_write_random_info.f90'

END module class_random
