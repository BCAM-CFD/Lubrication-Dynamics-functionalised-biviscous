!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------------------
SUBROUTINE write_file_info(this, file)
  !----------------------------------------------------------
  IMPLICIT NONE
  TYPE(file_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in) :: file
  LOGICAL :: opened

  !-- The file to be written with the information is opened --
  OPEN(file%unit, FILE=trim(file%name), ACCESS='APPEND')

  !-- We check if it is opened --
  INQUIRE(unit=this%unit, opened = opened) 

  WRITE(file%unit,*) 'Assigned unit ', this%unit
  WRITE(file%unit,*) 'File name     ', trim(this%name)
  IF (opened) THEN
     WRITE(file%unit,*) 'unit opened'
  ELSE
     WRITE(file%unit,*) 'unit closed'
  ENDIF
  WRITE(file%unit,*)

  CLOSE(file%unit)

END SUBROUTINE write_file_info
