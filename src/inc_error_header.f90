!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-------------------------------------------
SUBROUTINE error_header(file_name)
!-------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: file_name
  
  WRITE(*,*) 
  WRITE(*,*) '----------------------------------'
  WRITE(*,*) 'Error in file ', trim(file_name)
  WRITE(*,*) '----------------------------------'

END SUBROUTINE error_header
