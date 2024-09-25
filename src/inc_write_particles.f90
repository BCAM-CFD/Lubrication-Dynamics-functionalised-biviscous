!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!    BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!    at UNED, in Madrid, Spain.
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!    Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_particles(this, step, error_out)
  !---------------------------------------------
  ! The particles data is written out in the file "particles..."
  !---------------------------------------------
  use class_files_utilities
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: step
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I
  CHARACTER(LEN=MAX_CHAR) :: formatting
  INTEGER :: unit
  INTEGER :: bulk

  error_out = 0

  CALL update_name(this%output%particles, step)

  !--- The formatting is calculated ---
  IF (this%dim == 2) THEN
     formatting = '(I8,5E20.10, I8)'
  ELSE
     formatting = '(I8,7E20.10, I8)'
  ENDIF

  CALL search_unit(unit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  OPEN(unit, FILE=trim(this%output%particles%name), &
       FORM='FORMATTED', STATUS='UNKNOWN')
  DO I = 1, this%N
     IF (this%part(I)%bulk) THEN
        bulk = 1
     ELSE
        bulk = 0
     ENDIF
     WRITE(unit, formatting)           &    !2D    !3D
          I,                           &    !1     !1
          this%part(I)%pos,            &    !2,3   !2,3,4
          this%part(I)%vel(1:3),       &    !4,5   !5,6,7
          this%part(I)%R,              &    !6     !8
          bulk                              !7     !9
  ENDDO

  CLOSE(unit)

1000 CONTINUE  

END SUBROUTINE write_particles
