!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!------------------------------------------------------
REAL(Pr) FUNCTION random_Gaussian1(this) 
  !----------------------------------------------------
  ! Return a normal/Gaussian distributed random number.
  ! According to Numerical Recipes 2nd C.
  ! Taken from mcf (Xin Bian's SDPD code).
  !----------------------------------------------------
  IMPLICIT NONE
  TYPE(random_type), INTENT(INOUT)     :: this

  !----------------------------------------------------
  ! Local variables.
  !----------------------------------------------------

  REAL(Pr)                        :: v1,v2
  REAL(Pr)                        :: fac,rsq

  IF ( this%iset == 0 ) THEN

     DO

        CALL RANDOM_NUMBER(v1)
        CALL RANDOM_NUMBER(v2)
        v1 = 2.0_Pr * v1 - 1.0_Pr
        v2 = 2.0_Pr * v2 - 1.0_Pr
        rsq = v1**2 + v2**2

        !----------------------------------------------
        ! Stop if (v1,v2) are in unit circle.
        !----------------------------------------------

        IF ( rsq > 0.0_Pr .AND. rsq < 1.0_Pr ) THEN
           EXIT
        END IF

     END DO

     fac  = SQRT(-2.0*LOG(rsq)/rsq)

     !-------------------------------------------------
     ! Now make the Box-Muller transformation to get
     ! two normal deviates.
     ! Return one and save the other for next time.
     !-------------------------------------------------
     random_Gaussian1 = v1 * fac

     this%iset = 1
     this%gset = v2 * fac

  ELSE

     this%iset = 0
     random_Gaussian1 = this%gset

  END IF
  
END FUNCTION random_Gaussian1
