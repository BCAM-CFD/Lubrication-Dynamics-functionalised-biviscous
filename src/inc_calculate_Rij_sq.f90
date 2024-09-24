!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!--------------- inc_calculate_Rij_sq --------------
!----------------------------------------------------
! Here we calculate the square distance of two particles I and J.
!  You need to declare and define all the variables shown below
!----------------------------------------------------

  Pos_ij(:) = this%part(I)%Pos(:) - this%part(J)%Pos(:)
  !-- Periodic conditions --
  !-- In dim coordinate there are walls, no PBC --
  Pos_ij(1:dim-1) = Pos_ij(1:dim-1) - ANINT(Pos_ij(1:dim-1)/Box(1:dim-1))*Box(1:dim-1)  
  Rij_sq    = DOT_PRODUCT(Pos_ij, Pos_ij)

!--------------- end inc_calculate_Rij_sq --------------
