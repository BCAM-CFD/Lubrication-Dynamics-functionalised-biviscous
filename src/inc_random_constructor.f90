!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!--------------------------------------------------------
SUBROUTINE random_constructor(this, unit)
!--------------------------------------------------------
  ! Constructor de la clase random
  !------------------------------------------------------
  IMPLICIT NONE
  TYPE(random_type), INTENT(inout) :: this
  INTEGER, INTENT(in)              :: unit
  INTEGER, DIMENSION(8) :: time_array

  IF (this%fixed_seed) THEN
     WRITE(unit,*) '*** Seed is fixed ***'
  ENDIF

  CALL init_random_seed(this%fixed_seed, this%seed)
  WRITE(unit,*) '*** Random number generator initialized ***'
  WRITE(unit,*) '*** seed = ',this%seed,' ***'

  this%iset = 0

END SUBROUTINE random_constructor
