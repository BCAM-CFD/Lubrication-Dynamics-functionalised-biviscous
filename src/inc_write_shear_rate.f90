!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_shear_rate(this, step)
  !---------------------------------------------
  ! Calculated shear rate is written in the file 'shear_rate.dat'
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: step
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !--- The formatting is calculated ---
  formatting = '(I10, 2E20.10)'

  WRITE(this%output%shear_rate%unit, formatting)   &    
       step,                                  &    !1   
       this%calc_gamma_dot                         !2

END SUBROUTINE write_shear_rate
