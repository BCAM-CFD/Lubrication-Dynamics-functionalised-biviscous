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
