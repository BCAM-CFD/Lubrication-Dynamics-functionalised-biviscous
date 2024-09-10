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
