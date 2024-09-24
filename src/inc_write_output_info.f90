!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------------------
SUBROUTINE write_output_info(this, file, error_out)
  !----------------------------------------------------------
  ! The info about the output object is written out in the file "info.dat"
  !----------------------------------------------------------
  use class_file
  use class_files_utilities
  IMPLICIT NONE
  TYPE(output_type), INTENT(in) :: this
  TYPE(file_type), INTENT(in)   :: file
  INTEGER, INTENT(out)          :: error_out
  INTEGER :: unit

  !-- We look for a free unit number --
  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The file to be written with the information is opened --
  OPEN(unit, FILE=trim(file%name), ACCESS='APPEND')

  WRITE(unit,*)   
  WRITE(unit,*) '--------- output info --------------'

  WRITE(unit,*) 'dir = ', trim(this%dir)
  WRITE(unit,*)
  WRITE(unit,*) '--- Info file ---'
  CLOSE(unit)
  CALL write_file_info(this%info, file)
  OPEN(unit, FILE=trim(file%name), ACCESS='APPEND')
  WRITE(unit,*) '--- Particles file ---'
  CLOSE(unit)
  CALL write_file_info(this%particles, file)
  WRITE(unit,*) '--- Walls file ---'
  CLOSE(unit)
  CALL write_file_info(this%walls, file)
  WRITE(unit,*) '--- Nsweeps file ---'
  CLOSE(unit)
  CALL write_file_info(this%Nsweeps, file)
  WRITE(unit,*) '--- Stress file ---'
  CLOSE(unit)
  CALL write_file_info(this%stress, file)
  WRITE(unit,*) '--- Comp_time file ---'
  CLOSE(unit)
  CALL write_file_info(this%comp_time, file)

1000 CONTINUE

END SUBROUTINE write_output_info
