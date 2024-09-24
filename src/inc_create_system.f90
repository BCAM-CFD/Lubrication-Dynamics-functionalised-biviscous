!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------
SUBROUTINE create_system(this, input, error_out)
!------------------------------------------------
  ! The system is created.
  ! Simulation variables are constructed and initialized 
  ! in this subroutine.
  !----------------------------------------------
  use class_input
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this 
  TYPE(input_type), INTENT(in)        :: input
  INTEGER, INTENT(out)                :: error_out
  INTEGER  :: I         !-- Index --
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Vwall

  error_out = 0

  file_name = 'inc_create_system.f90'

  !--- Some checkings --------
  if (input%dim == 2) THEN
     error_out = 1
     CALL error_header(file_name)
     WRITE(*,*) '*** Create system error: &
          the code has been only checked for dim = 3. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF
  IF (input%N .LE. 0) THEN
     error_out = 1
     CALL error_header(file_name)
     WRITE(*,*) '*** Create system error: &
          N should be a positive number. ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF
  DO I = 1, input%dim
     IF (input%L(I) .LE. 0) THEN
        error_out = 1
        CALL error_header(file_name)
        WRITE(*,*) '*** Create system error: &
             L should be made of positive numbers. ***'
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDDO
  IF (input%dim .NE. 3) THEN
     error_out = 1
     CALL error_header(file_name)
     WRITE(*,*) '*** Create system error: the code is &
          checked only in 3D. ***'
     GOTO 1000 !-- End of subroutine ---
  ENDIF

  this%dim = input%dim
  this%pi  = 4.0_Pr * ATAN(1.0_Pr) 

  this%biviscous = input%biviscous
  this%eta0 = input%eta0
  this%eta1 = input%eta1
! JELA Viscosity Parametrisation
  this%L_visc = input%L_visc
! JELA Viscosity Parametrisation
  this%R_par = input%R_par
  this%a_par = input%a_par
  this%gamma_dot_critical = input%gamma_dot_critical
  this%V = input%V
  this%d_bulk = input%d_bulk
  this%L_bulk = input%d_bulk * input%L(input%dim)
  IF (input%dim == 2) THEN
     this%V_bulk = input%L(1) * (input%L(2) - 2.0_Pr * this%L_bulk)
  ELSE !-- dim == 3 --
     this%V_bulk = input%L(1) * input%L(2) * (input%L(3) - 2.0_Pr * this%L_bulk)
  ENDIF

  IF ((this%d_bulk <= 0) .OR. (this%d_bulk > 1)) THEN
     error_out = 1
     CALL error_header(file_name)
     WRITE(*,*) '*** Create system error: d_bulk should be greater &
          than 0 and lower than 1 ***'
     GOTO 1000 !-- End of subroutine --
  ENDIF

  this%sweep_tol   = input%sweep_tol
  this%N_sweep_max = input%N_sweep_max
  
  this%Nsteps = input%Nsteps
  this%dt     = input%dt
  this%dt_sq  = input%dt**2.0_Pr

  !-- Random object is initialized --
  ! We could call inc_read_random, or do the next --
  this%random%seed        = input%seed
  this%random%fixed_seed  = input%fixed_seed
  CALL init_random_seed(this%random%fixed_seed, this%random%seed)

  ALLOCATE(this%L(input%dim))
  this%L(1:input%dim)  = input%L(1:input%dim)
  this%gamma_dot = input%gamma_dot
  !-- The wall object is constructed --
  CALL wall_constructor(this%wall, input%dim, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  !-- The bottom wall will move with velocit -Vx_wall
  !   The top one with velocity +Vx_wall  ---
  ALLOCATE(Vwall(input%dim))
  Vwall(:) = 0.0_Pr
  Vwall(1) = input%L(input%dim) * input%gamma_dot / 2.0_Pr
  CALL initial_wall(this%wall, Vwall, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  this%explicit = input%explicit

  !-- About stress differences --
  ALLOCATE(this%Spp(input%dim, input%dim))
  ALLOCATE(this%force_plane_x(input%dim))
  ALLOCATE(this%force_plane_y(input%dim))
  ALLOCATE(this%force_plane_z(input%dim))

  this%rcut       = input%rcut
  this%rcut_sq    = input%rcut**2.0_Pr
  this%rcut_on    = input%rcut_on
  this%rcut_on_sq = input%rcut_on**2.0_Pr

  !-- rmax is calculated --
  this%rlist   = input%rlist
  this%rmax    = input%rcut * input%rlist
  this%rmax_sq = this%rmax**2.0_Pr

  !-- The number of sweeps for the semi-implicit method is initialized --
  this%N_sweep = 1

  this%dir_output = input%dir_output

  !-- Particles are build ---
  this%read_pos = input%read_pos
  this%file_pos = input%file_pos
  this%read_vel = input%read_vel
  this%file_vel = input%file_vel
  CALL initial_positions(this, input%N, input%R, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF
  CALL initial_mass(this, input%mass)
  CALL initial_velocities(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Some checkings --
  CALL check_pos_walls(this, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- Repulsive force --
  this%tau_rep = input%tau
  this%F0_rep = input%F0
  
  !-- Cells for neighbours calculation are initialized --
  CALL initial_cells(this, input%R, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  !-- The output object is initialized --
  CALL create_output(this%output, input%dir_output, &
       input%freq_write, input%freq_write_part, error_out)
  IF (error_out .NE. 0) THEN
     GOTO 1000 !-- End of subroutine --
  ENDIF

  WRITE(*,*) '*** System created ***'

1000 CONTINUE

  IF (ALLOCATED(Vwall)) THEN
     DEALLOCATE(Vwall)
  ENDIF

END SUBROUTINE create_system
