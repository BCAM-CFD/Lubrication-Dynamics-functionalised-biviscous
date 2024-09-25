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

!----------------------------------------------
SUBROUTINE write_cells_info(this)
!----------------------------------------------
  ! This subroutine writes in the shell information about all
  ! the cells.
  !--------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER :: I, J, K
  
  IF (ALLOCATED(this%cell)) THEN
     DO I = 1, this%Ncells(1)
        DO J = 1, this%Ncells(2)
           DO K = 1, this%Ncells(3)
              WRITE(*,*) '--- Cell', I, J, K,'---'
              CALL cell_info(this%cell(I, J, K))
              WRITE(*,*) 
           ENDDO
        ENDDO
     ENDDO
  ELSE
     WRITE(*,*) '*** Cell array is not allocated ***'
  ENDIF
  
END SUBROUTINE write_cells_info

