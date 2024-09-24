!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!---------------------------------------------
SUBROUTINE Check_update_neigh(this)
  !-----------------------------------------------
  ! This subroutine decides if the list of
  ! neighbours should be updated
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER                :: I
  REAL(Pr)               :: Disp_max1, Disp_max2
  REAL(Pr)               :: DispI_sq, Disp_max
  REAL(Pr), DIMENSION(3) :: Disp_vect
  
  Disp_Max1 = 0.0_Pr  !-- Greatest displacement --
  Disp_Max2 = 0.0_Pr  !-- Second greatest displacement --

  DO I = 1, this%N
     Disp_vect(:) = this%part(I)%Pos(:) - this%part(I)%Pos0(:)
     DispI_sq = DOT_PRODUCT(Disp_vect(:), Disp_vect(:))
     IF(DispI_sq > Disp_max1) THEN
        Disp_max2 = Disp_max1
        Disp_max1 = DispI_sq
     ELSE
        IF( DispI_sq > Disp_max2) THEN
           Disp_max2 = DispI_sq
        ENDIF
     ENDIF
  ENDDO
  
  !-- In this moment, Disp_max1 and Disp_max2 are the two greatest
  ! displacementes elevated square. --
  Disp_max = Sqrt(Disp_max1)+Sqrt(Disp_max2)
  !Si Disp_max > Rlist-Rcut hay que actualizar
  this%update_neigh = (Disp_max > (this%Rmax - this%Rcut)) 

END SUBROUTINE Check_update_neigh
