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

!---------------------------------------------
SUBROUTINE write_force_plane(this, step)
  !---------------------------------------------
  ! Data about the calculated forces in planes is written out
  ! in the file "plane.dat".
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: step
  CHARACTER(LEN=MAX_CHAR) :: formatting

  !--- The formatting is calculated ---
  IF (this%dim == 2) THEN
     formatting = '(I10, 6E20.10)'
  ELSE
     formatting = '(I10, 9E20.10)'
  ENDIF

  WRITE(this%output%plane%unit, formatting)   &    !2D    !3D
       step,                                  &    !1     !1
       this%force_plane_x,                    &    !2,3   !2,3,4
       this%force_plane_y,                    &    !4,5   !5,6,7
       this%force_plane_z                          !6,7   !8,9,10

END SUBROUTINE write_force_plane
