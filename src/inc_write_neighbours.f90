!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE write_neighbours(this, part)
!---------------------------------------------
  ! Subroutine to write in the shell the 
  ! of particle part neighbours
  !-------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER, INTENT(in)           :: part
  INTEGER :: I, J
  
  WRITE(*,*) '---- List of particle',part,'neighbours ----'
  DO I = 1, this%part(part)%N_neigh
     J = this%part(part)%neigh_list(I)
     WRITE(*,*) J
  ENDDO

END SUBROUTINE write_neighbours
