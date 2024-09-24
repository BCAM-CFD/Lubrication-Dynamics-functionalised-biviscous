!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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
