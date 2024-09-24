!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-----------------------------------------
SUBROUTINE cell_info(this)
!-----------------------------------------
  ! This subroutine writes in the shell information
  ! about the cell object.
  !---------------------------------------
  IMPLICIT NONE
  TYPE(cell_type), INTENT(in) :: this
  INTEGER :: I

  IF (ALLOCATED(this%min_coord)) THEN
     WRITE(*,*) 'min_coord ', this%min_coord
  ELSE
     WRITE(*,*) 'min_coord not allocated'
  ENDIF

  IF (ALLOCATED(this%max_coord)) THEN
     WRITE(*,*) 'max_coord ', this%max_coord
  ELSE
     WRITE(*,*) 'max_coord not allocated'
  ENDIF

  WRITE(*,*) 'N_neigh   ', this%N_neigh
  WRITE(*,*) 'Npart     ', this%Npart
  
  IF (ALLOCATED(this%neigh_coord)) THEN
     IF (this%N_neigh > 0) THEN
        DO I = 1, this%N_neigh
           WRITE(*,*) 'neigh_coord ',I,':',this%neigh_coord(I,:)
        ENDDO
     ENDIF
  ELSE
     WRITE(*,*) 'neigh_coord not allocated'
  ENDIF

  IF (ALLOCATED(this%list_part)) THEN
     WRITE(*,*) '--- List of particles ---'
     IF (this%Npart > 0) THEN
        DO I = 1, this%Npart
           WRITE(*,*) this%list_part(I)
        ENDDO
     ELSE
        WRITE(*,*) 'No particles'
     ENDIF
  ELSE
     WRITE(*,*) 'list_part NOT ALLOCATED'
  ENDIF
        


END SUBROUTINE cell_info
