!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!   BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
!
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!   at UNED, in Madrid, Spain.
!
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!   Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

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
