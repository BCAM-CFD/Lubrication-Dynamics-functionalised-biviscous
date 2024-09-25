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

!-----------------------------------------------------
SUBROUTINE destroy_output(this)
  !-----------------------------------------------------
  ! Destructor of the class output
  !---------------------------------------------------
  IMPLICIT NONE
  TYPE(output_type), INTENT(inout) :: this
  LOGICAL :: opened

  INQUIRE(unit=this%info%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%info%unit)
  ENDIF

  INQUIRE(unit=this%particles%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%particles%unit)
  ENDIF
  
  INQUIRE(unit=this%walls%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%walls%unit)
  ENDIF

  INQUIRE(unit=this%Nsweeps%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%Nsweeps%unit)
  ENDIF

  INQUIRE(unit=this%stress%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%stress%unit)
  ENDIF

  INQUIRE(unit=this%comp_time%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%comp_time%unit)
  ENDIF

  INQUIRE(unit=this%shear_rate%unit, opened = opened) !-- If the system is using it --
  IF (opened) THEN
     CLOSE(this%shear_rate%unit)
  ENDIF


END SUBROUTINE destroy_output
