!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-----------------------------------------------
SUBROUTINE VV_explicit(this, first_step, error_out)
!-----------------------------------------------
  ! This subroutine computes
  ! accelerations of all the relevant variables,
  ! and move the sistem using a DPD-Velocity Verlet
  ! integrator (See the next article:
  ! 'Dissipative particle dynamics: Bridging the gap 
  ! between atomistic and mesoscopic simulation' by
  ! Robert D. Groove and Patrick B. Warren).
  !----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  LOGICAL, INTENT(in)              :: first_step
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I
  
  !-- Accelerations are calculated for the first time step --
  IF (first_step) THEN
     CALL forces_explicit(this, error_out)
     IF (error_out .NE. 0) THEN
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDIF

  !-- The integrator moves the particles --
  DO I = 1, this%N
     !--  r(t+dt) is computed --
     this%part(I)%pos(:) = this%part(I)%pos(:) + &
          this%dt * this%part(I)%vel(:) + &
          0.5_Pr * this%dt_sq * this%part(I)%acc(:)

     !-- \tilde{v}(t+dt) is computed --
     this%part(I)%vel(:) = this%part(I)%vel(:) + &
          0.5_Pr * this%dt * this%part(I)%acc(:)
  ENDDO


  !-- Periodic boundary conditions are checked --
  CALL PBC(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF


  !-- Accelerations are calculated --
  CALL forces_explicit(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !--- The integrator---
  DO I = 1, this%N
     !-- v(t + dt) is computed --
     this%part(I)%vel(:) = this%part(I)%vel(:) + &
          0.5_Pr * this%dt * this%part(I)%acc(:)
  ENDDO

1000 CONTINUE

END SUBROUTINE VV_EXPLICIT
