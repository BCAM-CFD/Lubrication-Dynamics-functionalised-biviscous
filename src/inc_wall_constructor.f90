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

!-----------------------------------------
SUBROUTINE wall_constructor(this, dim, error_out)
!-----------------------------------------
  ! Constructor of class wall
  !---------------------------------------
  IMPLICIT NONE
  TYPE(wall_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: dim
  INTEGER, INTENT(out)           :: error_out
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_wall_constructor.f90'  

  IF (dim .NE. 3) THEN
     error_out = 1
     CALL error_header(file_name)     
     WRITE(*,*) '*** Particle constructor error: the code has been only &
          checked in 3D. ***'
     GOTO 1000 !-- End of subroutine ---
  ENDIF
     
  ALLOCATE(this%vel_top(dim))
  ALLOCATE(this%force_top(dim))
  ALLOCATE(this%lub_force_top(dim))
  ALLOCATE(this%rep_force_top(dim))
  ALLOCATE(this%vel_bottom(dim))
  ALLOCATE(this%force_bottom(dim))
  ALLOCATE(this%lub_force_bottom(dim))
  ALLOCATE(this%rep_force_bottom(dim))
  
1000 CONTINUE

END SUBROUTINE wall_constructor
