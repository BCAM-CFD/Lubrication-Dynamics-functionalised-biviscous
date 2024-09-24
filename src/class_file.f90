!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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
