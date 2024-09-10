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

!-----------------------------------------------------
SUBROUTINE read_input(this, input_file, error_out)
  !-----------------------------------------------------
  ! Subroutine to read the input file. This file should be
  ! edited if more input variables are added.
  !-----------------------------------------------------
  use class_read_input
  IMPLICIT NONE
  TYPE(input_type), INTENT(inout)     :: this
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: input_file
  INTEGER, INTENT(out)                :: error_out
  CHARACTER(LEN=MAX_CHAR) :: var_char
  REAL(Pr)  :: L_visc, gamma_dot0

  var_char = 'dim'
  CALL read_variable(input_file, var_char, this%dim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'R'
  CALL read_variable(input_file, var_char, this%R, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'mass'
  CALL read_variable(input_file, var_char, this%mass, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'N'  
  CALL read_variable(input_file, var_char, this%N, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'L'
  CALL read_variable(input_file, var_char, this%L, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'fixed_seed'
  CALL read_variable(input_file, var_char, this%fixed_seed, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'seed'
  CALL read_variable(input_file, var_char, this%seed, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'dir_output'
  CALL read_variable(input_file, var_char, this%dir_output, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'Nsteps'
  CALL read_variable(input_file, var_char, this%Nsteps, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'dt'
  CALL read_variable(input_file, var_char, this%dt, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'rcut'
  CALL read_variable(input_file, var_char, this%rcut, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'rcut_on'
  CALL read_variable(input_file, var_char, this%rcut_on, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'rlist'
  CALL read_variable(input_file, var_char, this%rlist, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'gamma_dot'
  CALL read_variable(input_file, var_char, this%gamma_dot, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'biviscous'
  CALL read_variable(input_file, var_char, this%biviscous, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'd_bulk'
  CALL read_variable(input_file, var_char, this%d_bulk, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'read_pos'
  CALL read_variable(input_file, var_char, this%read_pos, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'file_pos'
  CALL read_variable(input_file, var_char, this%file_pos, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'read_vel'
  CALL read_variable(input_file, var_char, this%read_vel, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'file_vel'
  CALL read_variable(input_file, var_char, this%file_vel, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'tau'
  CALL read_variable(input_file, var_char, this%tau, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'F0'
  CALL read_variable(input_file, var_char, this%F0, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'freq_write'
  CALL read_variable(input_file, var_char, this%freq_write, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'freq_write_part'
  CALL read_variable(input_file, var_char, this%freq_write_part, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'eta0'
  CALL read_variable(input_file, var_char, this%eta0, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'eta1'
  CALL read_variable(input_file, var_char, this%eta1, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'R_par'
  CALL read_variable(input_file, var_char, this%R_par, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'a_par'
  CALL read_variable(input_file, var_char, this%a_par, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  
  var_char = 'gamma_dot0'
  CALL read_variable(input_file, var_char, this%gamma_dot0, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
 
! JELA - BEGIN Viscosity parametrisation 
  var_char = 'L_visc'
  CALL read_variable(input_file, var_char, this%L_visc, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
! JELA - END Viscosity parametrisation

! Check-prints 
!print*, 'eta1 read = ', this%eta1
!print*, 'L_visc read = ', this%L_visc
!print*, 'gamma_dot0 read = ', this%gamma_dot0
!Stop
 
  var_char = 'gamma_dot_critical'
  CALL read_variable(input_file, var_char, this%gamma_dot_critical, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'V'
  CALL read_variable(input_file, var_char, this%V, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'sweep_tol'
  CALL read_variable(input_file, var_char, this%sweep_tol, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'N_sweep_max'
  CALL read_variable(input_file, var_char, this%N_sweep_max, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  var_char = 'explicit'
  CALL read_variable(input_file, var_char, this%explicit, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  WRITE(*,*) '*** Input read ***'

1000 CONTINUE

END SUBROUTINE read_input
