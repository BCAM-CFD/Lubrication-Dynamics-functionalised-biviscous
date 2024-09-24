!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE stress(this, error_out)
  !-----------------------------------
  ! The average stress tensor on the particles in the bulk is 
  ! computed in order to calculate the normal stress diferences.
  ! The stress is calculated with the Irving-Kirkwood method.
  ! Check Bertevas et al, Rheological acta, 2010 and
  ! Phan-Thien et al, Journal of rheology, 2014.
  ! **** Previously to compute the stress, the particles
  ! from the bulk should be calculated ***
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K, L, N_part
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  INTEGER :: dim
  REAL(Pr) :: Rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vi
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_norm
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_tang
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: F_rep_mag
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: F_nor_mag
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: F_tan_mag
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: F_tot_mag
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: F_ref_mag
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: R0_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: I_part
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: J_part
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp
  
  REAL(Pr) :: eij_vij
  REAL(Pr) :: eij_vij_old
  REAL(Pr) :: F0
  REAL(Pr) :: exp_taus
  REAL(Pr) :: s
  REAL(Pr) :: h
  REAL(Pr) :: R
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  REAL(Pr) :: Rij0 !dummy variable to temp. store Rij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name -- 
  REAL(Pr) :: gamma_dot_s_max
  REAL(Pr) :: h0_lim
  REAL(Pr) :: h0
  REAL(Pr) :: rmax
  REAL(Pr) :: gmgc
  REAL(Pr) :: P
  REAL(Pr) :: Q
  REAL(Pr) :: r1
  REAL(Pr) :: r2
  REAL(Pr) :: h_r1
  REAL(Pr) :: h_r2 

  error_out = 0

  file_name = 'inc_stress.f90'  

  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr
  fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr
  gij  = -6.0_Pr * this%pi * this%eta0 * R / 6.0_Pr

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(Fij(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(vi(dim))
  ALLOCATE(Flub_norm(dim))
  ALLOCATE(Flub_tang(dim))
  ALLOCATE(Spp(dim,dim))
  ALLOCATE(F_rep_mag(this%N * 50))
  ALLOCATE(F_nor_mag(this%N * 50))
  ALLOCATE(F_tan_mag(this%N * 50))
  ALLOCATE(F_tot_mag(this%N * 50))
  ALLOCATE(F_ref_mag(this%N * 50))
  ALLOCATE(R0_ij(this%N * 50))
  ALLOCATE(I_part(this%N * 50))
  ALLOCATE(J_part(this%N * 50))

  Box(:) = this%L(:)

  !-- Forces are initialized ---
  DO I = 1, this%N
     this%part(I)%Spp(:,:) = 0.0_Pr
  ENDDO
  
  DO I = 1, this%N*50
     F_rep_mag(I) = 0.0_Pr
     F_nor_mag(I) = 0.0_Pr
     F_tan_mag(I) = 0.0_Pr
     F_tot_mag(I) = 0.0_Pr
     F_ref_mag(I) = 0.0_Pr
     R0_ij(I) = 0.0_Pr
     I_part(I) = -1.0_Pr
     J_part(I) = -1.0_Pr
  ENDDO
  N_part=1

  !-- Forces and stresses are calculated --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        IF ((this%part(I)%bulk) .OR. (this%part(J)%bulk)) THEN

           include 'inc_calculate_Rij_sq.f90'

           IF (Rij_sq .LE. this%rcut_sq) THEN
              Rij = SQRT(Rij_sq) 
              Rij0 = Rij
              eij(:) = pos_ij(:) / Rij
              IF (Rij < this%rcut_on) THEN
                 Rij = this%rcut_on
              ENDIF
              vij(:) = this%part(I)%vel(:) - this%part(J)%vel(:)
              eij_vij = DOT_PRODUCT(eij, vij)
              eij_vij_old = eij_vij

              s = Rij - this%part(I)%R - this%part(J)%R
              h = s
              IF (s .LE. 0) THEN
                 CALL error_header(file_name)                 
                 WRITE(*,*) '*** stress error: particles',I,'and', J,&
                      'are penetrating one to each other. ***'
                 error_out = 1
                 GOTO 1000 !-- End of subroutine --
              ENDIF
              
				gamma_dot_s_max = ABS(9.0_Pr / 8.0_Pr * (ABS(eij_vij_old)/h) * SQRT((3.0_Pr / 2.0_Pr) * (this%a_par / h)))
!
!! JELA: BEGINNING - h0lim Introduce gdot_c(h)				
!				h0_lim = 3.0_Pr/4.0_Pr*((9.0_Pr/2.0_Pr)*eij_vij_old**2.0_Pr*this%a_par/(this%gamma_dot_critical**2.0_Pr))**(1.0_Pr/3.0_Pr) ! Original
                                h0_lim = 243.0_Pr/128.0_Pr * (eij_vij_old**2.0_Pr * this%a_par / (this%gamma_dot_critical**2.0_Pr) ) ! Inverse GammaDotC w/gamma_dot_critical
!! JELA: END - h0lim Introduce gdot_c(h)
!
				h0 = h
				rmax = SQRT(2.0_Pr/3.0_Pr*this%a_par*h0)
!
!! JELA: BEGINNING - gmgc Introduce gdot_c(h)
!				gmgc= gamma_dot_s_max/this%gamma_dot_critical ! Original
				gmgc= gamma_dot_s_max/(this%gamma_dot_critical/h0)
!! JELA: END - gmgc Introduce gdot_c(h)
!
				P = (-1.0_Pr + (2.0_Pr*gmgc**2.0_Pr) + (2.0_Pr*gmgc*SQRT(-1.0_Pr + gmgc**2.0_Pr)))**(1.0_Pr/3.0_Pr)
				Q = SQRT(-1.0_Pr + (1.0_Pr / P)  +  P)
				r1 = rmax*(Q- SQRT(-3.0_Pr - Q**2.0_Pr  + (4.0_Pr * gmgc/ Q) ) )
				r2 = rmax*(Q+ SQRT(-3.0_Pr - Q**2.0_Pr  + (4.0_Pr * gmgc/ Q) ) )
				
				h_r1 = h0*(1.0_Pr + r1**2.0_Pr / (2.0_Pr*this%a_par*h0))
				h_r2 = h0*(1.0_Pr + r2**2.0_Pr / (2.0_Pr*this%a_par*h0))
				
				!###### The force is given by the forces written below as
				!#    F = Fm0,  if h0 >= h0_lim
				!#    F = Fb,   if h0 < h0_lim
				!# Fm1 is not used
!				#########################################################
				fij1=0.0_Pr
				IF (h0 .GE. h0_lim ) THEN	
					!# Monoviscous force with eta = eta0
!
!!  JELA: BEGINNING - F1 Introduce eta0(h) 
!					fij1 = 6.0_Pr * this%pi * eij_vij_old * this%a_par**2.0 * this%eta0 / h0 ! Original
					fij1 = 6.0_Pr * this%pi * eij_vij_old * this%a_par**2.0 * (this%eta0*EXP(-h0/this%L_visc)+this%eta1) / h0
!!  JELA: END - F1 Introduce eta0(h) 
!
! Check-prints 
!print*, 'eta1 stress = ', this%eta1
!print*, 'L_visc stress = ', this%L_visc
!print*, 'gamma_dot0 stress = ', this%gamma_dot0
!Stop
!
				ELSE
					!# Biviscous Force
!
!!  JELA: BEGINNING - F2 Introduce gdot_c(h) & eta0(h) 
!       Original
!					fij1 = 2.0_Pr * this%pi * ( (3.0_Pr * this%a_par**2.0 * this%eta0 * eij_vij_old * (1.0_Pr/ h0)) + &
!					(3.0_Pr * (this%eta0 - this%eta1) * eij_vij_old * this%a_par**2.0 * &
!					(h_r2**(-1.0) - h_r1**(-1.0)) * (2.0 - h0 * (h_r1**(-1.0) + h_r2**(-1.0))) ) + &
!					(2.0 * this%a_par * this%gamma_dot_critical * (this%eta0 - this%eta1) * &
!					( (r2- r1) + ( (h_r1- 2.0 * h0) * SQRT(2.0 * this%a_par/h0) * &
!				   (ATAN(r2/SQRT(2.0 * this%a_par * h0)) - ATAN(r1/ SQRT(2.0 * this%a_par * h0)) ) ) ) ) )
!
!        Inverse gdot_c(h) w/gamma_dot_critical       
       fij1 = 2.0_Pr * this%pi * ( (3.0_Pr * this%a_par**2.0 * (this%eta0*EXP(-h0/this%L_visc)+this%eta1) * &
            eij_vij_old * (1.0_Pr/ h0)) + &
            (3.0_Pr * ((this%eta0*EXP(-h0/this%L_visc)+this%eta1) - this%eta1) * eij_vij_old * this%a_par**2.0 * &
            (h_r2**(-1.0) - h_r1**(-1.0)) * (2.0 - h0 * (h_r1**(-1.0) + h_r2**(-1.0))) ) + &
            (2.0 * this%a_par * (this%gamma_dot_critical/h0) * ((this%eta0*EXP(-h0/this%L_visc)+this%eta1) - this%eta1) * &
            ( (r2- r1) + ( (h_r1- 2.0 * h0) * SQRT(2.0 * this%a_par/h0) * &
            (ATAN(r2/SQRT(2.0 * this%a_par * h0)) - ATAN(r1/ SQRT(2.0 * this%a_par * h0)) ) ) ) ) )
!
!!  JELA: END - F2 Introduce gdot_c(h) & eta0(h)
!
                ENDIF

              !-- Lubrication force --
              !Flub_norm(:) = (fij1/s + fij2 * &
               !    LOG(this%part(I)%R / s)) * eij_vij * eij(:) 
                   
				!IF (s .LE. ((this%rcut- 2.0_Pr * this%part(I)%R)/5.0_Pr) ) THEN
				!	Flub_tang(:) = gij * LOG(this%part(I)%R / s) * &
				!	(vij(:) - eij_vij * eij(:))
				!ELSE
				!	Flub_tang(:) = 0.0_Pr
				!ENDIF
				Flub_norm(:) = -(fij1) * eij(:) 
				!WRITE(* , *) fij1, Flub_norm(1)				
				Flub_tang(:) = 0.0_Pr              
                   
              !-- Replusion force --
              eij(:) = pos_ij(:) / Rij0
              s = Rij0 - this%part(I)%R - this%part(J)%R
			  IF(s .LE. ((this%rcut_on- 2.0_Pr * this%part(I)%R)*0.0001_Pr))THEN
              s = (this%rcut_on- 2.0_Pr * this%part(I)%R)*0.0001_Pr
			  ENDIF
              
              IF (s .LE. 0) THEN
                 CALL error_header(file_name)                 
                 WRITE(*,*) '*** stress error: particles',I,'and', J,&
                      'are penetrating one to each other. ***'
                 error_out = 1
                 GOTO 1000 !-- End of subroutine --
              ENDIF
              exp_taus = EXP(-this%tau_rep * s)
              F0 = this%F0_rep * this%tau_rep * exp_taus / &
                   (1.0_Pr - exp_taus)
              Fij(:) = F0 * eij(:)
!Fij.dat variables
              R0_ij(N_part)   = Rij0 - this%part(I)%R - this%part(J)%R
              F_rep_mag(N_part) = F0
              F_nor_mag(N_part) = -fij1
              F_tan_mag(N_part) = 0.0_Pr
              F_tot_mag(N_part) = F_rep_mag(N_part) + F_nor_mag(N_part) +F_tan_mag(N_part)
              F_ref_mag(N_part) = -6.0_Pr * this%pi * eij_vij_old * this%a_par**2.0 * this%eta1 / h0
              I_part(N_part) = I
              J_part(N_part) = J
              N_part = N_part + 1
!Fij.dat ends              
              !sum forces
              Fij(:) = Fij(:) + Flub_norm(:) + Flub_tang(:)
              !WRITE(* , *) Flub_norm(1), Flub_tang(:), Fij(:)
              DO L = 1, dim
                 Spp(L,:) = pos_ij(L) * Fij(:)
              ENDDO
              this%part(I)%Spp = this%part(I)%Spp + Spp
              this%part(J)%Spp = this%part(J)%Spp + Spp
           ENDIF
        ENDIF
     ENDDO
  ENDDO

  !-- The stress tensor is calculated ---
  this%Spp(:,:) = 0.0_Pr
  DO I = 1, this%N
     IF (this%part(I)%bulk) THEN
        vi(:) = this%part(I)%vel(:)
        vi(1) = vi(1) - ((-0.5_Pr*this%L(dim)*this%calc_gamma_dot) + &
             this%calc_gamma_dot * this%part(I)%pos(dim)) 
        DO J = 1, dim
           Spp(J,:) = vi(J) * vi(:) * this%part(I)%mass 
        ENDDO
        this%Spp(:,:) = this%Spp(:,:) + &
             (0.5_Pr * this%part(I)%Spp(:,:)) + Spp(:,:)
     ENDIF
  ENDDO
  this%Spp = -this%Spp / this%V_bulk
  
  OPEN(UNIT=89, FILE= 'test/Fij.dat', ACTION="write", STATUS="replace")
  DO I=1,N_part-1
	 WRITE(89,'(8E20.10)') R0_ij(I), F_rep_mag(I), F_nor_mag(I), F_tan_mag(I), F_tot_mag(I), F_ref_mag(I), I_part(I), J_part(I)
  ENDDO
  CLOSE(UNIT=89)
  
 
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
  IF (ALLOCATED(Fij)) THEN
     DEALLOCATE(Fij)
  ENDIF
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  IF (ALLOCATED(vi)) THEN
     DEALLOCATE(vi)
  ENDIF
  IF (ALLOCATED(Flub_norm)) THEN
     DEALLOCATE(Flub_norm)
  ENDIF
  IF (ALLOCATED(Flub_tang)) THEN
     DEALLOCATE(Flub_tang)
  ENDIF
  IF (ALLOCATED(R0_ij)) THEN
     DEALLOCATE(R0_ij)
  ENDIF
  IF (ALLOCATED(F_rep_mag)) THEN
     DEALLOCATE(F_rep_mag)
  ENDIF
  IF (ALLOCATED(F_nor_mag)) THEN
     DEALLOCATE(F_nor_mag)
  ENDIF
  IF (ALLOCATED(F_tan_mag)) THEN
     DEALLOCATE(F_tan_mag)
  ENDIF
  IF (ALLOCATED(F_tot_mag)) THEN
     DEALLOCATE(F_tot_mag)
  ENDIF
  IF (ALLOCATED(F_ref_mag)) THEN
     DEALLOCATE(F_ref_mag)
  ENDIF
  IF (ALLOCATED(I_part)) THEN
     DEALLOCATE(I_part)
  ENDIF
  IF (ALLOCATED(J_part)) THEN
     DEALLOCATE(J_part)
  ENDIF
END SUBROUTINE stress
