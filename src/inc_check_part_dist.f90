!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-- Before this, you need to calculate the neighbour list --
!-----------------------------------------------
SUBROUTINE check_part_dist(this, error_out)
  !-----------------------------------------------
  ! Checking that the particles are not overlapping
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout)    :: this
  INTEGER, INTENT(out)                :: error_out
  INTEGER :: I, J, K
  INTEGER :: dim
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij(:)
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box(:)
  REAL(Pr) :: Rij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  
  
  error_out = 0

  file_name = 'inc_check_part_dist.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))

  Box(:) = this%L(:)

  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        
        include 'inc_calculate_Rij_sq.f90'
        Rij = SQRT(Rij_sq)
        IF (Rij .LE. this%part(I)%R + this%part(J)%R) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** Check particles distance error: the particles '
           WRITE(*,*) I, J, 'are overlapping each other. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
     ENDDO
  ENDDO

1000 CONTINUE

  DEALLOCATE(Box)
  DEALLOCATE(pos_ij)

END SUBROUTINE check_part_dist
