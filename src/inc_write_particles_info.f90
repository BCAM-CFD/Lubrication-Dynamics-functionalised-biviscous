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
