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

!-------------------------------------------------
  SUBROUTINE update_name(this, step)
!-------------------------------------------------
    ! We update the name of the file with the 
    ! corresponding time step.
    !---------------------------------------------
    IMPLICIT NONE
    TYPE(file_type), INTENT(inout) :: this
    INTEGER, INTENT(in)            :: step
    CHARACTER(LEN=MAX_CHAR) :: termination

    WRITE(termination,'(I10.10,A)')  step,'.dat'
    this%name = trim(this%base_name)//trim(termination)

  END SUBROUTINE update_name
