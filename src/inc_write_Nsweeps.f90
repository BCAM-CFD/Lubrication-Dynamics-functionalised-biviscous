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

!---------------------------------------------
SUBROUTINE write_Nsweeps(this, step)
  !---------------------------------------------
  ! Nsweeps data is written out in the file "Nsweeps.dat"
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)            :: step

  WRITE(this%output%Nsweeps%unit, '(2I10)')   &    
       step,                           &    !1
       this%N_sweep                         !2
  
END SUBROUTINE write_Nsweeps
