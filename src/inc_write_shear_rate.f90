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

!---------------------------------------------
SUBROUTINE write_shear_rate(this, step)
  !---------------------------------------------
  ! Calculated shear rate is written in the file 'shear_rate.dat'
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: step
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !--- The formatting is calculated ---
  formatting = '(I10, 2E20.10)'

  WRITE(this%output%shear_rate%unit, formatting)   &    
       step,                                  &    !1   
       this%calc_gamma_dot                         !2

END SUBROUTINE write_shear_rate
