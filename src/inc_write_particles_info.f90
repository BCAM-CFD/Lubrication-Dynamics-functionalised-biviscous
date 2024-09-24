!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------
SUBROUTINE write_particles_info(this)
!---------------------------------------
  ! Subroutine to write in the shell information
  ! about all the particles.
  !-------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER :: I

  IF (this%N == 0 .OR. .NOT.(ALLOCATED(this%part))) THEN
     WRITE(*,*) '** Sorry, there are not particles to display information. **'
  ELSE
     DO I = 1, this%N
        WRITE(*,*) '****** Particle',I,'***********'
        CALL particle_info(this%part(I))
        WRITE(*,*) 
     ENDDO
  ENDIF

END SUBROUTINE write_particles_info
