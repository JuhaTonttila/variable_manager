MODULE test_procedures
    USE mo_structured_datatypes
    USE mo_parameters
    IMPLICIT NONE

    CONTAINS

    SUBROUTINE compute_some_profile(SELF)
        CLASS(FloatArray), INTENT(inout) :: SELF

        INTEGER :: i
        
        SELECT TYPE(SELF)
        TYPE IS (FloatArray1d)
            IF (.NOT. ASSOCIATED(SELF%d)) WRITE(*,*) "onDemand not allocated"
            DO i = 1,nzp
                SELF%d(i) = i*10.
            END DO
        END SELECT

    END SUBROUTINE compute_some_profile


END MODULE test_procedures