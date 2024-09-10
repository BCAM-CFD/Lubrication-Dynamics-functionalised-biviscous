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

module class_input
  !--------------------------------
  ! Class related to input variables.
  ! This class allows to read the input
  ! variables from a file.
  !--------------------------------
  use class_computational
  IMPLICIT NONE

  TYPE input_type
     INTEGER  :: Nsteps    !-- Number of steps --
     REAL(Pr)  :: dt        !-- Time step --
     LOGICAL  :: explicit  !-- Scheme explicit (.TRUE.) or
                           !   implicit (.FALSE.) --
     REAL(Pr) :: sweep_tol !-- Tolerance. Defined in Vazquez-Quesada
                           ! et al, Journal of Non-Newtonian Fluid
                           ! Mechanics 2016.     
     INTEGER  :: N_sweep_max !-- Maximum allowed number of sweeps --
     INTEGER  :: dim       !-- Number of dimensions --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: L !-- Box size --
     REAL(Pr) :: R_par     !-- particle size --
     REAL(Pr) :: a_par         !-- particle reduced size --
     REAL(Pr) :: eta0      !-- Shear viscosity --
     REAL(Pr) :: eta1      !-- Viscosity at high shear rates (from the
                           !   bi-viscous model) --
! JELA - BEGIN Viscosity parametrisation
     REAL(Pr) :: L_visc !-- Char. lenght for particle separation  --
! JELA - END Viscosity parametrisation
     REAL(Pr) :: gamma_dot0 !-- Shear rate --
     REAL(Pr) :: gamma_dot_critical !-- critical Shear rate --
     REAL(Pr) :: gamma_dot !-- Shear rate --
     REAL(Pr) :: V         !-- particle relative velocity --
     LOGICAL  :: biviscous !-- If the fluid is biviscous or
                           !   monoviscous --
     REAL(Pr) :: d_bulk    !-- Distance from walls (in units of Lz) 
                           !   which we already consider as bulk. --
     INTEGER  :: N         !-- Number of particles --     
     REAL(Pr) :: R         !-- Radius of particles --
     REAL(Pr) :: mass      !-- Mass of particles --
     LOGICAL  :: read_pos  !-- If the position are read from a file --
     CHARACTER(LEN=MAX_CHAR) :: file_pos !-- File from the positions
                                         ! are read from --
     LOGICAL  :: read_vel  !-- If the vels are read from a file --
     CHARACTER(LEN=MAX_CHAR) :: file_vel !-- File from the vels
                                         ! are read from --
     REAL(Pr) :: rcut      !-- Cutoff radius --
     REAL(Pr) :: rcut_on   !-- Cuton radius --
     REAL(Pr) :: rlist     !-- To define the skin around rcut in order
                           ! to search neighbours. rlist should be 
                           ! > 1. The radius of the sphere to search
                           ! neighbours will be rlist * rcut
     REAL(Pr) :: tau       !-- Parameter from the repulsion force --
     REAL(Pr) :: F0        !-- Parameter from the repulsion force --
     LOGICAL :: fixed_seed !-- If the seed for the random numbers
                           !   generator is fixed --
     INTEGER :: seed       !-- Seed for the random numbers generator -
     CHARACTER(LEN=MAX_CHAR) :: dir_output !-- Directory to output
                               ! It will be created if it does not
                               ! exist --
     INTEGER  :: freq_write !-- Freq to write output --
     INTEGER  :: freq_write_part !-- Freq to write particles output --
  END type input_type

  !-------- SUBROUTINES AND FUNCTIONS ---------------
CONTAINS
  include 'inc_read_input.f90'
  include 'inc_input_destructor.f90'

end module class_input
