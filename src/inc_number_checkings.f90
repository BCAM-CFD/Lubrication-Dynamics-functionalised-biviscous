!--------------------------------------------------------
! Code developed by Adolfo Vazquez-Quesada and Marco Ellero.
! Institution: Zienkiewicz Centre for Computational
!               Engineering (ZCCE), Swansea University.
! Contact: adolfo.vazquez@swansea.ac.uk
!          marco.ellero@swansea.ac.uk
!--------------------------------------------------------

!--------------------- number_checkings -----------------------

    SELECT CASE(checking)
       CASE (1) !variable should be positive
          IF (variable .LE. 0) THEN
             WRITE(*,*) 
             WRITE(*,*) '*** Input error : ',trim(this%variable_name), &
                  ' should be positive. ***'
             WRITE(*,*)
             error_out = -1
          ENDIF
       CASE (2) !variable should not be negative
          IF (variable < 0) THEN
             WRITE(*,*) 
             WRITE(*,*) '*** Input error : ',trim(this%variable_name), &
                  ' should not be negative. ***'
             WRITE(*,*)
             error_out = -2
          ENDIF

       END SELECT

!----------------- END number_checkings -----------------------
