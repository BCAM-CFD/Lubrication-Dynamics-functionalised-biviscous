!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!   BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
!
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!   at UNED, in Madrid, Spain.
!
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!   Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

!-----------------------------------------------
SUBROUTINE VV(this, first_step, time, error_out)
!-----------------------------------------------
  ! This subroutine computes
  ! accelerations of all the relevant variables,
  ! and move the sistem using a DPD-Velocity Verlet
  ! integrator (with lambda = 0.5) (See the next article:
  ! 'Dissipative particle dynamics: Bridging the gap 
  ! between atomistic and mesoscopic simulation' by
  ! Robert D. Groove and Patrick B. Warren).
  !----------------------------------------------
  use class_comp_time
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  LOGICAL, INTENT(in)                 :: first_step
  TYPE(comp_time_type), INTENT(inout) :: time
  INTEGER, INTENT(out)                :: error_out
  INTEGER :: I
  REAL    :: t0, tf

  CALL cpu_time(t0)
  
  !-- Accelerations are calculated for the first time step --
  IF (first_step) THEN
     CALL forces(this, error_out)
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
  CALL forces(this, error_out)
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

  CALL cpu_time(tf)
  time%VV = time%VV + tf - t0

END SUBROUTINE VV
