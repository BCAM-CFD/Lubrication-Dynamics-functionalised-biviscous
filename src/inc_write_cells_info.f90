!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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

