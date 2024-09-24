!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!*********************************************
!  CLASS FUNCTIONS_UTILITIES SUBROUTINE
!*********************************************

!------------------------------------------------
SUBROUTINE mean_val(this, mean)
!------------------------------------------------
  ! The mean value of an array is obtained.
  !----------------------------------------------
  IMPLICIT NONE
  REAL(Pr), DIMENSION(:), INTENT(in) :: this
  REAL(Pr), INTENT(out)              :: mean
  INTEGER :: I

  mean = 0.0_Pr
  DO I = 1, SIZE(this)
     mean = mean + this(I)
  ENDDO
  mean = mean / REAL(SIZE(this), KIND = Pr)

END SUBROUTINE mean_val
