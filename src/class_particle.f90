!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

module class_particle
  !---------------------------------
  ! Class of the particles.
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE particle_type

     !-- Basic physic variables --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: pos   !-- position --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: vel   !-- velocity --
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: force !-- force ---
     REAL(Pr), DIMENSION(:), ALLOCATABLE   :: acc   !-- acc ---     
     REAL(Pr)                              :: R     !-- Radius ---
     REAL(Pr)                              :: mass  !-- mass --
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp   !-- interpart. stress ---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_rep   !-- interpart. stress (rep forces)---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_lub_norm   !-- interpart. stress (normal lub forces)---
     REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp_lub_tang   !-- interpart. stress (tang lub forces) ---

     !-- Neighbours variables --
     INTEGER, DIMENSION(:) , ALLOCATABLE :: neigh_list !-- neighbours list --
     REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos0       !-- Last position before updating neighbours --
     INTEGER                             :: N_neigh    !-- Number of neighbours --

     !-- Others --
     LOGICAL :: bulk               !-- If we consider it is in the bulk region --

  END type particle_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_particle_constructor.f90'
    include 'inc_particle_info.f90'
    include 'inc_particle_destructor.f90'
 
END module class_particle
