!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

module class_computational
  !---------------------------------
  ! Class related to SDPD particles
  !---------------------------------
  IMPLICIT NONE

  INTEGER, PARAMETER :: MAX_CHAR = 1000 !-- Max characters
                                        ! for string variables --
  INTEGER, PARAMETER :: Pr = 8 !-- real precision
                               ! (Pr = 4 is single precision, Pr = 8 is double precision) 

!!$  TYPE computational_type
!!$     
!!$  END type computational_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_error_header.f90'

  END module class_computational
