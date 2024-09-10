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

module class_cell
  !---------------------------------
  ! Class cell: class of the cells used to search for neighbours.
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE cell_type
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: min_coord  !-- Min coordinates of the cell --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: max_coord  !-- Max coordinates of the cell --
     INTEGER :: N_neigh                          !-- Number of neighbour cells --
     INTEGER, DIMENSION(:,:), ALLOCATABLE :: neigh_coord !-- Coordinates of the neighbour cells --
     INTEGER, DIMENSION(:), ALLOCATABLE   :: list_part !-- List of particles of the cell -
     INTEGER :: Npart !-- Number of particles in the cell --
  END type cell_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_cell_constructor.f90'
    include 'inc_cell_info.f90'
    include 'inc_cell_destructor.f90'
 
END module class_cell
