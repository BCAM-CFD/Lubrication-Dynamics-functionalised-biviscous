!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_Nsweeps(this, step)
  !---------------------------------------------
  ! Nsweeps data is written out in the file "Nsweeps.dat"
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: step

  WRITE(this%output%Nsweeps%unit, '(2I10)')   &    
       step,                           &    !1
       this%N_sweep                         !2
  
END SUBROUTINE write_Nsweeps
