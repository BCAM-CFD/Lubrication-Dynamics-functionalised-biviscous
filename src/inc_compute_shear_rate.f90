!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------
SUBROUTINE compute_shear_rate(this, error_out)
!------------------------------------------------
  ! This subroutine computes the shear rate from the
  ! particles from the bulk.
  !----------------------------------------------
  use class_functions_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: data
  INTEGER  :: Nbulk
  REAL(Pr) :: alpha
  REAL(Pr) :: r
  INTEGER  :: I


  !-- First, the data array is built --
  ALLOCATE(data(this%N,2))

  Nbulk = 0
  DO I = 1, this%N
     IF (this%part(I)%bulk) THEN
        Nbulk = Nbulk + 1
        data(Nbulk, 1) = this%part(I)%pos(this%dim)
        data(Nbulk, 2) = this%part(I)%vel(1)
     ENDIF
  ENDDO

  CALL lin_regression(data(1:Nbulk, 1:2), alpha, &
       this%calc_gamma_dot, r, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF


1000 CONTINUE

  IF (ALLOCATED(data)) THEN
     DEALLOCATE(data)
  ENDIF

END SUBROUTINE compute_shear_rate
