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

!*********************************************
!  CLASS FILES_UTILITIES FILE
!*********************************************
! It is not really a class. Only a compilation
! of several utilities about files managing.
!*********************************************

!-----------------------------------------------------
module class_files_utilities
  !---------------------------------
  use class_computational
  IMPLICIT NONE

  INTERFACE file_to_array
     module PROCEDURE file_to_array_real, &
          file_to_array_double          , &
          file_to_array_integer
  END INTERFACE file_to_array

  CONTAINS
    include 'inc_obtain_list_files.f90'
    include 'inc_obtain_list_dirs.f90'
    include 'inc_search_unit.f90'
    include 'inc_file_number_lines.f90'
    include 'inc_file_number_columns.f90'
    include 'inc_file_to_array.f90'
    include 'inc_check_directory.f90'

  END module class_files_utilities

!-- If you are going to remove this class you should delete the
!   next files. --
! class_files_utilities.f90
! inc_check_directory.f90
! inc_file_number_columns.f90
! inc_file_number_lines.f90
! inc_file_to_array.f90
! inc_obtain_list_dirs.f90
! inc_obtain_list_files.f90
! inc_search_unit.f90

