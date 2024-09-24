!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!*********************************************
!  CLASS FUNCTIONS_UTILITIES
!*********************************************
! This is not really a class, but a group of 
! utilities related to functions.
!-----------------------------------------------------

module class_functions_utilities
  use class_computational
  IMPLICIT NONE
  
  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_mean_value.f90'
    include 'inc_lin_regression.f90'
    include 'inc_bit_function.f90'
    include 'inc_data_delimiter.f90'

  END module class_functions_utilities
