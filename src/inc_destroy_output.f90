!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

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
