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

module class_comp_time
  !---------------------------------
  ! Class related to computational times
  !---------------------------------
  use class_computational
  IMPLICIT NONE
  
  TYPE comp_time_type
     
     REAL(Pr) :: total     !-- Total time --
     REAL(Pr) :: neigh     !-- time to search for neighbours --
     REAL(Pr) :: semi_impl !-- time to do the semi-implicit method --
     REAL(Pr) :: VV        !-- time to move the simulation
                           !   (with the Velocity Verlet integrator)

  END type comp_time_type

  !------- SUBROUTINES AND FUNCTIONS --------------
  CONTAINS
    include 'inc_initialize_comp_time.f90'
    include 'inc_write_comp_times.f90'
 
END module class_comp_time
