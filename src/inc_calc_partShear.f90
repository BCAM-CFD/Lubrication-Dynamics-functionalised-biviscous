!---------------------------------------------------------------------
! This code has been developed  in a collaboration between
!
! - Marco Ellero, leader of the  CFD Modelling and Simulation group at
!   BCAM (Basque Center for Applied Mathematics) in Bilbao, Spain
!
! - Adolfo Vazquez-Quesada, from the Department of Fundamental Physics
!   at UNED, in Madrid, Spain.
!
! - Jose Esteban  Lopez Aguilar,  from the Departamento  de Ingenieria
!   Quimica at UNAM, in Mexico DF, Mexico.
!
! Developers: Adolfo Vazquez-Quesada.
!             Jose Esteban Lopez-Aguilar.
!---------------------------------------------------------------------

!---------------------------------------------
SUBROUTINE calc_partShear(this, error_out)
  !-----------------------------------
  ! Calculations of forces on particles (only repulsion forces).
  ! See Vazquez-Quesada et al, Journal of Non-Newtonian Fluid Mechanics, 2016.
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K, L , N_part
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  INTEGER :: dim
  REAL(Pr) :: Rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vi
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vj
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: part_gammaDot
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: h0_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: R0_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: h0_lim
  REAL(Pr) :: s
  REAL(Pr) :: eij_vij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_calc_partShear.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(vi(dim))
  ALLOCATE(vj(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(h0_ij(this%N * 50))
  ALLOCATE(part_gammaDot(this%N * 50))
  ALLOCATE(R0_ij(this%N * 50))
  ALLOCATE(h0_lim(this%N * 50))

  Box(:) = this%L(:)
  N_part = 1
  
  DO I = 1, this%N*50
     h0_ij(I) = -1.0_Pr
     R0_ij(I) = -1.0_Pr
     part_gammaDot(I) = -1.0_Pr
     h0_lim(I) = -1.0_Pr
  ENDDO
  
  !-- The forces are computed --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        IF ((this%part(I)%bulk) .OR. (this%part(J)%bulk)) THEN
        include 'inc_calculate_Rij_sq.f90'

        IF (Rij_sq .LE. this%rcut_sq) THEN
           Rij = SQRT(Rij_sq) 
           IF (Rij < this%part(I)%R + this%part(J)%R) THEN
              CALL error_header(file_name)              
              WRITE(*,*) '*** forces error: particles',I,'and', J,&
                   'are penetrating one to each other. ***'
              error_out = 1
              GOTO 1000 !-- End of subroutine --
           ENDIF
        R0_ij(N_part) = Rij 
		s = Rij - this%part(I)%R - this%part(J)%R
		IF(s .LE. ((this%rcut_on- 2.0_Pr * this%part(I)%R)*1_Pr) )THEN
			s = (this%rcut_on- 2.0_Pr * this%part(I)%R)*1_Pr
		ENDIF
		h0_ij(N_part)   = s
		vi(:)   = this%part(I)%vel(:)
		vj(:)   = this%part(J)%vel(:)
		vij(:)  = vi(:) - vj(:)
		eij(:)  = pos_ij(:) / Rij
        eij_vij = DOT_PRODUCT(eij, vij)
        part_gammaDot(N_part) = (9.0_Pr / 8.0_Pr) * (eij_vij / s) * SQRT(1.5_Pr * this%a_par / s)
!
!! JELA - BEGINNING h0lim functional
!  h0_lim(N_part) = 3.0_Pr/4.0_Pr*((9.0_Pr/2.0_Pr)*(eij_vij**2.0_Pr)*this%a_par/(this%gamma_dot_critical**2.0_Pr))**(1.0_Pr/3.0_Pr) ! Original
 h0_lim(N_part) = 243.0_Pr/128.0_Pr*(eij_vij**2.0_Pr)*this%a_par/(this%gamma_dot_critical**2.0_Pr) ! Inverse w/ gamma_dot_critical
!! JELA - END h0lim functional
!
		N_part = N_part + 1
		ENDIF
		ENDIF
	ENDDO
  ENDDO
 
  OPEN(UNIT=88, FILE= 'test/gammaDot.dat', ACTION="write", STATUS="replace")
  DO I=1,N_part-1
        WRITE(88,'(4E20.10)') h0_ij(I), R0_ij(I), ABS(part_gammaDot(I)), h0_lim(I)
  ENDDO
  CLOSE(UNIT=88)

1000 CONTINUE

  IF (ALLOCATED(Box)) THEN
     DEALLOCATE(Box)
  ENDIF
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(eij)) THEN
     DEALLOCATE(eij)
  ENDIF
  IF (ALLOCATED(vi)) THEN
     DEALLOCATE(vi)
  ENDIF
  IF (ALLOCATED(vj)) THEN
     DEALLOCATE(vj)
  ENDIF
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  IF (ALLOCATED(part_gammaDot)) THEN
     DEALLOCATE(part_gammaDot)
  ENDIF
  IF (ALLOCATED(h0_ij)) THEN
     DEALLOCATE(h0_ij)
  ENDIF
  IF (ALLOCATED(h0_lim)) THEN
     DEALLOCATE(h0_lim)
  ENDIF
  IF (ALLOCATED(R0_ij)) THEN
     DEALLOCATE(R0_ij)
  ENDIF
END SUBROUTINE calc_partShear
