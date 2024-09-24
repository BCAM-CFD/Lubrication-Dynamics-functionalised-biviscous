!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!-------------------------------------------
  SUBROUTINE particle_info(this)
!-------------------------------------------
    ! This subroutine writes in the shell information
    ! about the particle object.
    !---------------------------------------
    IMPLICIT NONE
    TYPE(particle_type), INTENT(in) :: this
    INTEGER :: I
    
    IF (ALLOCATED(this%pos)) THEN
       WRITE(*,*) 'Pos     = ', this%pos(:)
    ELSE
       WRITE(*,*) 'Pos not allocated'
    ENDIF

    IF (ALLOCATED(this%vel)) THEN
       WRITE(*,*) 'Vel     = ', this%vel(:)
    ELSE
       WRITE(*,*) 'Vel not allocated'
    ENDIF

    IF (ALLOCATED(this%force)) THEN
       WRITE(*,*) 'Force   = ', this%force(:)
    ELSE
       WRITE(*,*) 'Force not allocated'
    ENDIF

    WRITE(*,*) 'R       = ', this%R
    WRITE(*,*) 'mass    = ', this%mass

    IF (ALLOCATED(this%pos0)) THEN
       WRITE(*,*) 'Pos0    = ', this%pos0(:)
    ELSE
       WRITE(*,*) 'Pos0 not allocated'
    ENDIF

    WRITE(*,*) 'N_neigh = ', this%N_neigh    

    WRITE(*,*) '---- List of neighbours ----'
    IF (this%N_neigh .NE. 0) THEN
       DO I = 1, this%N_neigh
          WRITE(*,*) this%neigh_list(I)
       ENDDO
    ENDIF

  END SUBROUTINE particle_info
