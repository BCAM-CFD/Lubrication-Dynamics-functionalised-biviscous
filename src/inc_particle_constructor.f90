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

!-----------------------------------------
SUBROUTINE particle_constructor(this, dim, Nmax, error_out)
!-----------------------------------------
  ! Constructor of class particle
  !---------------------------------------
  IMPLICIT NONE
  TYPE(particle_type), INTENT(inout) :: this
  INTEGER, INTENT(in)                :: dim
  INTEGER, INTENT(out)               :: error_out
  INTEGER, INTENT(in)                :: Nmax
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_particle_constructor.f90'
 

  IF (dim .NE. 3) THEN
     error_out = 1
     CALL error_header(file_name)     
     WRITE(*,*) '*** Particle constructor error: the code has been only &
          checked in 3D. ***'
     GOTO 1000 !-- End of subroutine ---
  ENDIF
     
  ALLOCATE(this%pos(dim))
  ALLOCATE(this%vel(dim))
  ALLOCATE(this%force(dim))
  ALLOCATE(this%acc(dim))
  ALLOCATE(this%pos0(dim))
  ALLOCATE(this%Spp(dim, dim))
  ALLOCATE(this%Spp_rep(dim, dim))
  ALLOCATE(this%Spp_lub_norm(dim, dim))
  ALLOCATE(this%Spp_lub_tang(dim, dim))
  ALLOCATE(this%neigh_list(Nmax))
  
1000 CONTINUE

END SUBROUTINE particle_constructor
