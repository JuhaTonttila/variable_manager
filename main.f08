PROGRAM main
    USE classFieldArray, ONLY : FieldArray
    USE mo_structured_datatypes
    USE test_procedures
    USE mo_parameters

    ! Test list:
    ! different storage options
    ! Grouping, fetching by group
    ! Fetching by output status

    IMPLICIT NONE
    ! Test the polymorphic variable declarations

    CHARACTER(len=6), PARAMETER :: recGrp = "record"
    CHARACTER(len=6), PARAMETER :: axesGrp = "axes"

    TYPE(FieldArray) :: manager

    TYPE(FloatArray1d), TARGET :: external1d
    TYPE(FloatArray1d), TARGET :: store1d
    TYPE(FloatArray1d), TARGET :: od1d

    REAL, TARGET :: data1d(nzp)
    REAL, TARGET :: data1d_2(nzp)

    TYPE(FieldArray) :: dim_manager

    TYPE(FloatArray1d), TARGET :: zt
    TYPE(FloatArray0d), TARGET :: time 

    REAL, TARGET :: dimdata(nzp) ! just 1d here...

    CLASS(FloatArray), POINTER :: pipeline 

    INTEGER :: k


    pipeline => NULL()

    data1d = 1000000.
    data1d_2 = 2000000.

    DO k = 1,nzp
        dimdata(k) = REAL(k)
    END DO

    ! Initialize variable manager instance
    manager = FieldArray()
    dim_manager = FieldArray()

    ! Initialize dimension variables and register with dim manager
    time = FloatArray0d("time")
    CALL time%allocate_internal([1])
    pipeline => time
    CALL dim_manager%newField(time%shortname,["time"],[1],[1],[0],pipeline,      &
                              outputstatus=.TRUE.,long_name="time",unit="s",     &
                              group=[recGrp])  ! Assigning recGrp makes this the record variable. 
                                             ! This can only have one variable, otherwise the 
                                             ! IO routines will fail at least for netcdf.

    pipeline => NULL()
    zt = FloatArray1d("zt", trgt=dimdata)
    pipeline => zt 
    CALL dim_manager%newField(zt%shortName,["zt"],[nzp],[nzp],[0],pipeline,      &
                              outputstatus=.TRUE.,long_name="zt",unit="m",       &
                              group=[axesGrp])
   

    ! Initialize variables and register with the manager
    external1d = FloatArray1d("v1d_1", trgt=data1d)
    pipeline => external1d
    CALL manager%newField(external1d%shortName,["zt  ","time"],[nzp,1],[nzp,1],[0,0],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan",unit="v1d unit")

    pipeline => NULL()
    store1d = FloatArray1d("v1d_2")
    CALL store1d%allocate_internal([nzp])
    store1d%d(:) = 1000.
    pipeline => store1d
    CALL manager%newField(store1d%shortName,["zt  ","time"],[nzp,1],[nzp,1],[0,0],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan 2",unit="v1d unit 2")
                   
    od1d = FloatArray1d("v1d_3",func=compute_some_profile)
    pipeline => od1d
    CALL manager%newField(od1d%shortName,["zt  ","time"],[nzp,1],[nzp,1],[0,0],pipeline,  &
                          outputstatus=.TRUE.,long_name="v1d testaan 3",unit="v1d unit 3")


    CALL make_h5(dim_manager, manager, time, recGrp, axesGrp)

    !CALL make_nc(dim_manager, manager, time, recGrp, axesGrp)



    !CALL test(manager)

    pipeline => NULL()
    CALL store1d%free_memory()
    WRITE(*,*) "od1d loppu ", ASSOCIATED(od1d%d)

    CONTAINS

    SUBROUTINE test(mngr)
        TYPE(FieldArray), INTENT(inout) :: mngr

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


    SUBROUTINE make_nc(dim_mngr,mngr,time_,recGrp_,axesGrp_)
        USE classOStreamNCDF, ONLY : OStreamNCDF
        TYPE(FieldArray), INTENT(inout) :: dim_mngr
        TYPE(FieldArray), INTENT(inout) :: mngr
        TYPE(FloatArray0d), INTENT(inout) :: time_  
        CHARACTER(len=*), INTENT(in) :: recGrp_, axesGrp_

        TYPE(FieldArray) :: record,axes
        TYPE(OStreamNCDF) :: ncid

        INTEGER :: t
 
        ncid = OStreamNCDF("testi.nc")

        CALL ncid%open_nc()

        CALL ncid%define_dimensions(dim_mngr,recGrp_)

        CALL ncid%define_variables(dim_mngr)
        CALL ncid%define_variables(mngr)

        CALL ncid%finalize_init()

        CALL dim_mngr%getByGroup(recGrp_,record)  ! Extract the record variable in its own FieldArray instance
        CALL dim_mngr%getByGroup(axesGrp_,axes)   ! Check the comments in the hdf5 version, this probably could be 
                                                  ! omitted with Netcdf, but it shouldn't hurt either. 

        CALL ncid%write_var(axes)  ! Write out the non-record variables. Only needed once

        DO t = 1,5
            time_%d = REAL(t)   !!! Incrementing time would be in the timestepping loop of the model
            ! These 3 are the actual call that would be in the output routine
            CALL ncid%newRecord(record)  !! %newRecord must be called before writing out variables with a record dimension
            CALL ncid%write_var(mngr)
            CALL ncid%sync_nc()
        END DO 

        CALL ncid%close_nc()

    END SUBROUTINE make_nc

    ! ----------------------------------------------------------

    SUBROUTINE make_h5(dim_mngr,mngr,time_,recGrp_,axesGrp_)
        USE classOStreamHDF5, ONLY : OStreamHDF5  
        TYPE(FieldArray), INTENT(inout) :: dim_mngr
        TYPE(FieldArray), INTENT(inout) :: mngr
        TYPE(FloatArray0d), INTENT(inout) :: time_  
        CHARACTER(len=*), INTENT(in) :: recGrp_
        CHARACTER(len=*), INTENT(in) :: axesGrp_

        TYPE(FieldArray) :: record,axes
        TYPE(OStreamHDF5) :: h5id

        INTEGER :: t

        h5id = OStreamHDF5("testi.h5")

        CALL h5id%open_h5()

        CALL h5id%define_dimensions(dim_mngr,recGrp_)

        CALL h5id%define_variables(mngr)

        CALL dim_mngr%getByGroup(recGrp_,record)  ! Extract the record variable in its own FieldArray instance

        CALL dim_mngr%getByGroup(axesGrp_,axes)   ! Extract axes vectors in its own FieldArray instance. This enables
                                                  ! writing out the axes (which don't contain record dimension!) without
                                                  ! calling the new record first. These only have to be written out ONCE.
        CALL h5id%write_var(axes)
        DO t = 1,5
            time_%d = REAL(t*3300.)
            CALL h5id%newRecord(record)
            CALL h5id%write_var(mngr)
        END DO 

        CALL h5id%close_h5()


    END SUBROUTINE make_h5




END PROGRAM main