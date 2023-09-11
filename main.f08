PROGRAM main
    USE classFieldArray, ONLY : FieldArray
    USE mo_structured_datatypes
    USE test_procedures
    USE mo_parameters
    IMPLICIT NONE
    ! Test the polymorphic variable declarations

    TYPE(FieldArray) :: manager

    TYPE(FloatArray1d), TARGET :: external1d
    TYPE(FloatArray1d), TARGET :: store1d
    TYPE(FloatArray1d), TARGET :: od1d

    REAL, TARGET :: data1d(nzp)
    

    TYPE(FieldArray) :: dim_manager

    TYPE(FloatArray1d), TARGET :: zt

    REAL, TARGET :: dimdata(nzp) ! just 1d here...

    CLASS(FloatArray), POINTER :: pipeline 

    INTEGER :: k

    pipeline => NULL()

    data1d = 1000000.

    DO k = 1,nzp
        dimdata(k) = REAL(k)
    END DO

    ! Initialize variable manager instance
    manager = FieldArray()
    dim_manager = FieldArray()

    ! Initialize axis variables and register with dim manager
    zt = FloatArray1d("zt", trgt=dimdata)
    pipeline => zt 
    CALL dim_manager%newField(zt%shortName,["zt"],[nzp],[nzp],[1],pipeline,      &
                              outputstatus=.TRUE.,long_name="zt",unit="m")



    ! Initialize variables and register with the manager
    external1d = FloatArray1d("v1d_1", trgt=data1d)
    pipeline => external1d
    CALL manager%newField(external1d%shortName,["zt"],[nzp],[nzp],[1],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan",unit="v1d unit")

    store1d = FloatArray1d("v1d_2")
    CALL store1d%allocate_internal([nzp])
    store1d%d(:) = 1000.
    pipeline => store1d
    CALL manager%newField(store1d%shortName,["zt"],[nzp],[nzp],[1],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan 2",unit="v1d unit 2")
                   
    od1d = FloatArray1d("v1d_3",func=compute_some_profile)
    pipeline => od1d
    CALL manager%newField(od1d%shortName,["zt"],[nzp],[nzp],[1],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan 3",unit="v1d unit 3")

    CALL make_nc(dim_manager, manager)

    CALL test(manager)

    pipeline => NULL()
    CALL store1d%free_memory()
    WRITE(*,*) "od1d loppu ", ASSOCIATED(od1d%d)

    CONTAINS

    SUBROUTINE test(mngr)
        TYPE(FieldArray), INTENT(in) :: mngr

        CLASS(FloatArray), POINTER :: pp 
        pp => NULL()

        WRITE(*,*) "n 1"
        CALL mngr%getVarInst(pp,1,1)
        SELECT TYPE(pp)
            TYPE IS (FloatArray1d)
                WRITE(*,*) pp%d, ASSOCIATED(pp%d)
        END SELECT
        CALL pp%free_memory()  !! Would lead to segfaul with %d associated to external storage. 
                               !! Instead writes a warning and skips the called procedure.

        WRITE(*,*) "n 2"
        CALL mngr%getVarInst(pp,2,1)
        SELECT TYPE(pp)
            TYPE IS (FloatArray1d)
                WRITE(*,*) pp%d, ASSOCIATED(pp%d)
        END SELECT
        CALL pp%free_memory()  !! Memory allocated directly to pointer so technically valid
        

        WRITE(*,*) "n 3"
        CALL mngr%getVarInst(pp, 3, 1)  
        CALL pp%allocate_internal(mngr%getCountsLocal(3))
        CALL pp%onDemand()
        SELECT TYPE (pp) 
            TYPE IS (FloatArray1d)
                WRITE(*,*) pp%d, ASSOCIATED(pp%d)
        END SELECT
        CALL pp%free_memory() !! Memory allocated directly to pointer so technically valid

        pp => NULL()

    END SUBROUTINE test


    SUBROUTINE make_nc(dim_mngr,mngr)
        USE classOStreamNCDF, ONLY : OStreamNCDF
        TYPE(FieldArray), INTENT(in) :: dim_mngr
        TYPE(FieldArray), INTENT(in) :: mngr 

        TYPE(OStreamNCDF) :: ncid

        INTEGER :: t
 
        ncid = OStreamNCDF("testi.nc")

        CALL ncid.open_nc()

        CALL ncid.define_record("time","time","s")
        CALL ncid.define_dimensions(dim_mngr)
        CALL ncid.define_variables(mngr)

        DO t = 1,5
            CALL ncid.write_new_record("time",t*60.)
            CALL ncid.write_var(mngr)
            CALL ncid.sync_nc()
        END DO 

        CALL ncid.close_nc()

    END SUBROUTINE make_nc



END PROGRAM main