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

 module class_file
   !--------------------------------
   ! Class related to files
   !--------------------------------
   use class_computational
   IMPLICIT NONE

   TYPE file_type
      INTEGER                 :: unit
      CHARACTER(LEN=MAX_CHAR) :: name
      CHARACTER(LEN=MAX_CHAR) :: base_name
   END type file_type

   !-------- SUBROUTINES AND FUNCTIONS ---------------
   CONTAINS
     include 'inc_file_constructor.f90'
     include 'inc_update_name.f90'
     include 'inc_file_destructor.f90'

     !-- For output --
     include 'inc_write_file_info.f90'

 end module class_file
