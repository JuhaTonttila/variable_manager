MODULE classOStreamNCDF
    USE netcdf

    TYPE OStreamNCDF

        INTEGER :: ncid = 0
        INTEGER :: nrec = 0
        INTEGER :: nvar
        CHARACTER(len=150) :: fname

        CONTAINS

        PROCEDURE :: open_nc,           &
                     sync_nc,           &
                     close_nc,          &
                     define_record,     &
                     define_dimensions, &
                     define_variables,  &
                     write_new_record,  &
                     write_var

        PROCEDURE, PRIVATE :: get_dimids


    END TYPE OStreamNCDF
    INTERFACE OStreamNCDF
        PROCEDURE constructor
    END INTERFACE OStreamNCDF


    CONTAINS

    ! ------------------------------

    FUNCTION constructor(fname)
        CHARACTER(len=*), INTENT(in) :: fname
        TYPE(OStreamNCDF) :: constructor
        constructor%fname = fname
    END FUNCTION constructor

    ! ------------------------------

    SUBROUTINE open_nc(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        INTEGER :: iret
        iret = nf90_create(SELF%fname,NF90_SHARE,SELF%ncid)
    END SUBROUTINE open_nc

    ! ------------------------------

    SUBROUTINE sync_nc(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        INTEGER :: iret
        iret = nf90_sync(SELF%ncid)
    END SUBROUTINE sync_nc

    ! ------------------------------

    SUBROUTINE close_nc(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF 
        INTEGER :: iret 
        iret = nf90_close(SELF%ncid) 
    END SUBROUTINE close_nc 

    ! ------------------------------

    SUBROUTINE define_record(SELF,name,longname,unit)
        ! ----------------------------------------------
        ! Define the record variable
        ! ---------------------------
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        CHARACTER(len=*), INTENT(in) :: name,longname,unit
        INTEGER :: err, dimid, did
        err = nf90_def_dim(SELF%ncid, name, NF90_UNLIMITED, dimid)
        err = nf90_def_var(SELF%ncid, name, NF90_FLOAT, dimid, did)
        err = nf90_put_att(SELF%ncid, did, 'longname', longname)
        err = nf90_put_att(SELF%ncid, did, 'units'   , unit)      
    END SUBROUTINE define_record

    ! -------------------------------

    SUBROUTINE define_dimensions(SELF,dims)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        TYPE(FieldArray), INTENT(in) :: dims
        INTEGER :: i, n, err, did

        n = dims%count
        DO i = 1,n
            err = nf90_def_dim(SELF%ncid, dims%getName(i), &
                               dims%getCountsLocal(i), did)
        END DO

    END SUBROUTINE define_dimensions

    ! -------------------------------

    SUBROUTINE define_variables(SELF,vars)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        TYPE(FieldArray), INTENT(in) :: vars
        INTEGER :: i,err,did
        INTEGER, ALLOCATABLE :: dimid(:)

        ! Check if vars%count > 0?

        DO i = 1,vars%count
            CALL SELF%get_dimids(vars%getDimension(i),dimid)
            err = nf90_def_var(SELF%ncid, vars%getName(i), NF90_FLOAT, dimid, did )
            err = nf90_put_att(SELF%ncid, did, 'longname', vars%getLongName(i))
            err = nf90_put_att(SELF%ncid, did, 'unit', vars%getUnit(i))
            DEALLOCATE(dimid)
        END DO

    END SUBROUTINE define_variables

    ! -------------------------------

    SUBROUTINE get_dimids(SELF,dims,ids)
        CLASS(OStreamNCDF), INTENT(in) :: SELF
        CHARACTER(len=*), INTENT(in) :: dims(:)
        INTEGER, ALLOCATABLE, INTENT(out) :: ids(:)
        INTEGER :: n,i,err

        n = SIZE(dims)
        ALLOCATE(ids(n))
        ids = 0
        DO i = 1,n
            err = nf90_inq_dimid(SELF%ncid, dims(i), ids(i))
        END DO

    END SUBROUTINE get_dimids

    ! ------------

    SUBROUTINE write_new_record(SELF,name,val)
        CLASS(OStreamNCDF) :: SELF
        CHARACTER(len=*), INTENT(in) :: name
        REAL, INTENT(in) :: val
        INTEGER :: err,did
        SELF%nrec = SELF%nrec + 1
        err = nf90_inq_varid(SELF%ncid,name,did)
        err = nf90_put_var(SELF%ncid,did,val,start=SELF%nrec)
    END SUBROUTINE write_new_record

    ! -------------

    SUBROUTINE write_var(SELF,vars)
        USE classFieldArray, ONLY : FieldArray
        USE mo_structured_datatypes
        CLASS(OStreamNCDF), INTENT(in) :: SELF
        TYPE(FieldArray), INTENT(in) :: vars

        CLASS(FloatArray), POINTER :: pp
        INTEGER :: err, n, i, did
        INTEGER, ALLOCATABLE :: beg(:), count(:)

        pp => NULL()
        n = vars%count

        DO i = 1,n
            beg = [vars%getOffsets(i),SELF%nrec]
            count = [vars%getCountsLocal(i),1]  
            CALL vars%getVarInst(pp, i, 1)

            IF (ASSOCIATED(pp%onDemand)) THEN
                CALL pp%allocate_internal(vars%getCountsLocal(i))
                CALL pp%onDemand()
            END IF

            err = nf90_inq_varid(SELF%ncid,vars%getName(i),did)
            SELECT TYPE(pp)
                TYPE IS (FloatArray0d)
                    err = nf90_put_var(SELF%ncid,did,pp%d,start=beg,count=count)
                TYPE IS (FloatArray1d)                   
                    err = nf90_put_var(SELF%ncid,did,pp%d,start=beg,count=count)
                TYPE IS (FloatArray2d)                 
                    err = nf90_put_var(SELF%ncid,did,pp%d,start=beg,count=count)
                TYPE IS (FloatArray3d)                   
                    err = nf90_put_var(SELF%ncid,did,pp%d,start=beg,count=count)
                TYPE IS (FloatArray4d)                  
                    err = nf90_put_var(SELF%ncid,did,pp%d,start=beg,count=count)
            END SELECT

            IF (ASSOCIATED(pp%onDemand)) CALL pp%free_memory()            

            pp => NULL()
            DEALLOCATE(beg,count)

        END DO

    END SUBROUTINE write_var


END MODULE classOStreamNCDF