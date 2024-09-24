!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!*********************************************
!  CLASS FILES_UTILITIES FILE
!*********************************************

!----------------------------------------------------
SUBROUTINE search_unit(unit, error_out, forbidden)
!----------------------------------------------------
  ! A usable unit for files is searched.
  ! forbidden is an array that tell is if one unit
  ! is forbidden because it is assigned to other file,
  ! even when it is not still used, in order to avoid 
  ! incompatibilities.
  !--------------------------------------------------
  IMPLICIT NONE
  INTEGER, INTENT(out)                           :: unit
  LOGICAL, DIMENSION(:), OPTIONAL, INTENT(inout) :: forbidden
  INTEGER, INTENT(inout)                         :: error_out
  INTEGER :: max_unit
  INTEGER :: I
  LOGICAL :: opened

  error_out = 0
  max_unit  = 1000

  IF (present(forbidden)) THEN
     IF (SIZE(forbidden) .NE. max_unit) THEN
        WRITE(*,*) '*** Search unit error: size of forbidden array should be equal to ', max_unit
        error_out = 1
        GOTO 1000 !-- End of subroutine --
     ENDIF
  ENDIF

  DO I = 1, max_unit
     INQUIRE(unit=I, opened = opened) !-- If the system is using it --
     IF (.NOT.(present(forbidden))) THEN
        IF (.NOT.(opened)) THEN
           unit = I
           GOTO 456
        ENDIF
     ELSE
        IF (.NOT.(opened) .AND. .NOT.(forbidden(I))) THEN
           unit = I
           forbidden(I) = .TRUE.
           GOTO 456
        ENDIF
     ENDIF
  ENDDO
  !-- If this line is reached, no free unit was found --
  WRITE(*,*) '*** Search unit error: no free unit has been found. ***'
  error_out = 1
456 CONTINUE

1000 CONTINUE

END SUBROUTINE search_unit
