!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

  module class_output
   !--------------------------------
   ! Class of output files
   !--------------------------------
   use class_computational
   use class_file
   IMPLICIT NONE

   TYPE output_type
      CHARACTER(LEN=MAX_CHAR) :: dir         !-- Directory to output --
      TYPE(file_type)         :: info        !-- General info file --
      TYPE(file_type)         :: particles   !-- particles data file --
      TYPE(file_type)         :: walls       !-- Walls data file --
      TYPE(file_type)         :: shear_rate  !-- Shear rate data file --
      TYPE(file_type)         :: Nsweeps     !-- Nsweeps data file --
      TYPE(file_type)         :: stress      !-- Stress data file --
      TYPE(file_type)         :: comp_time   !-- Computational data file --
      TYPE(file_type)         :: plane       !-- (stress on ) planes data file --
      INTEGER                 :: freq_write  !-- freq to output (but particles) --
      INTEGER                 :: freq_write_part !-- freq to output particles files --
   END type output_type

   !-------- SUBROUTINES AND FUNCTIONS ---------------
   CONTAINS
     include 'inc_create_output.f90'
     include 'inc_write_output_info.f90'
     include 'inc_destroy_output.f90'

 end module class_output
