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

!-------------------------------------------
SUBROUTINE error_header(file_name)
!-------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=MAX_CHAR), INTENT(in) :: file_name
  
  WRITE(*,*) 
  WRITE(*,*) '----------------------------------'
  WRITE(*,*) 'Error in file ', trim(file_name)
  WRITE(*,*) '----------------------------------'

END SUBROUTINE error_header
