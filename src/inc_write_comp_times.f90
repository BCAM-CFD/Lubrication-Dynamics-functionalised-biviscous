!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_comp_times(this, step, unit)
  !---------------------------------------------
  ! The computational times are outputted in a file.
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(comp_time_type), INTENT(in) :: this
  INTEGER, INTENT(in)              :: step
  INTEGER, INTENT(in)              :: unit

  WRITE(unit, '(I10, 4E20.10)') &
       step,               &  !1
       this%total,         &  !2
       this%neigh,         &  !3
       this%semi_impl,     &  !4
       this%VV                !5
  
END SUBROUTINE write_comp_times
