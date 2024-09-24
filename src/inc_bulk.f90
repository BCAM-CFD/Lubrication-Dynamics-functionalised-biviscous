!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------------
SUBROUTINE bulk(this)
!---------------------------------------------------
  ! We decide which particles are considered as bulk.
  !-------------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER :: I, J, K, L, M

  DO I = 1, this%N
     IF (this%part(I)%pos(this%dim) > this%L_bulk .AND. &
          this%part(I)%pos(this%dim) < this%L(this%dim) - this%L_bulk) THEN
        this%part(I)%bulk = .TRUE.
     ELSE
        this%part(I)%bulk = .FALSE.
     ENDIF
  ENDDO


END SUBROUTINE bulk
