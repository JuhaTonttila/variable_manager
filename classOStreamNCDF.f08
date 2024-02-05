MODULE classOStreamNCDF
    USE netcdf
    IMPLICIT NONE

    TYPE OStreamNCDF

        INTEGER :: ncid = 0         ! File ID handle for the stream
        INTEGER :: nrec = 0         ! Current number of records
        INTEGER :: recdid = -1    ! Record dimension ID (assume only 1, if any)
        INTEGER :: nvar             ! Number of variables defined for the stream
        CHARACTER(len=150) :: fname ! Output filename for the stream
        

        CONTAINS

        PROCEDURE :: open_nc,           &
                     sync_nc,           &
                     close_nc,          &
                     finalize_init,     &
                     newRecord,         &
                     define_dimensions, &
                     define_variables,  &
                     write_var

        PROCEDURE, PRIVATE :: get_dimids,    &
                              updateOffset,  &
                              findRecordIdx, &
                              excludeRecord


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
        SELF%nrec = 0  !! This has to have some additional stuff for opening pre-existing files
    END SUBROUTINE open_nc

    ! ------------------------------

    SUBROUTINE sync_nc(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        INTEGER :: iret
        iret = nf90_sync(SELF%ncid)
    END SUBROUTINE sync_nc

    ! ------------------------------
    SUBROUTINE finalize_init(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        INTEGER :: err
        err = nf90_enddef(SELF%ncid)
        CALL SELF%sync_nc()
    END SUBROUTINE finalize_init

    ! --------------------------------

    SUBROUTINE close_nc(SELF)
        CLASS(OStreamNCDF), INTENT(inout) :: SELF 
        INTEGER :: iret 
        iret = nf90_close(SELF%ncid) 
    END SUBROUTINE close_nc 


    ! ------------------------------------------------------------
    ! Update the record index. Call this before calling write_var
    !
    SUBROUTINE newRecord(SELF,record)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamNCDF) :: SELF
        TYPE(FieldArray), INTENT(inout) :: record
        SELF%nrec = SELF%nrec + 1 
        CALL SELF%write_var(record)
    END SUBROUTINE newRecord


    ! -------------------------------

    SUBROUTINE define_dimensions(SELF,dims,recGrp)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamNCDF), INTENT(inout) :: SELF
        TYPE(FieldArray), INTENT(inout) :: dims
        CHARACTER(len=*), INTENT(in), OPTIONAL :: recGrp
        INTEGER :: i, n, err, did
        CHARACTER(len=10) :: recg_ 
        INTEGER, ALLOCATABLE :: counts_(:)

        recg_ = ""
        IF (PRESENT(recGrp)) recg_ = recGrp

        n = dims%count
        DO i = 1,n
            IF (dims%isInGroup(recg_,i)) THEN
                err = nf90_def_dim(SELF%ncid, dims%getName(i), &
                                   NF90_UNLIMITED, did)
                SELF%recdid = did
            ELSE
                counts_ = dims%getCountsLocal(i)
                err = nf90_def_dim(SELF%ncid, dims%getName(i), &
                                   counts_(1), did)
                DEALLOCATE(counts_)
            END IF 

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
        ! Should also take care of the case where continuing pre-existing file

        DO i = 1,vars%count
            CALL SELF%get_dimids(vars%getDimension(i),dimid)
            err = nf90_def_var(SELF%ncid, vars%getName(i), NF90_FLOAT, dimid, did )
            err = nf90_put_att(SELF%ncid, did, 'longname', vars%getLongName(i))
            err = nf90_put_att(SELF%ncid, did, 'unit', vars%getUnit(i))
            DEALLOCATE(dimid)
        END DO

    END SUBROUTINE define_variables

    ! -------------------------------

    SUBROUTINE write_var(SELF,vars)
        USE classFieldArray, ONLY : FieldArray
        USE mo_structured_datatypes
        CLASS(OStreamNCDF), INTENT(in) :: SELF
        TYPE(FieldArray), INTENT(inout) :: vars

        CLASS(FloatArray), POINTER :: pp
        INTEGER :: err, n, i, varid
        INTEGER, ALLOCATABLE :: beg(:), count(:)

        pp => NULL()
        n = vars%count

        DO i = 1,n

            err = nf90_inq_varid(SELF%ncid,vars%getName(i),varid)

            CALL vars%getVarInst(pp, i, 1)
            beg = vars%getOffsets(i)
            count = vars%getCountsLocal(i)  

            IF (ASSOCIATED(pp%onDemand)) THEN
                CALL pp%allocate_internal(SELF%excludeRecord(count,varid))  
                CALL pp%onDemand()
            END IF
            CALL SELF%updateOffset(beg,varid)
            !! SET_OFFSET to update the variable instance??


            SELECT TYPE(pp)
                TYPE IS (FloatArray0d)
                    err = nf90_put_var(SELF%ncid,varid,pp%d,start=beg)
                TYPE IS (FloatArray1d)                   
                    err = nf90_put_var(SELF%ncid,varid,pp%d,start=beg,count=count)
                TYPE IS (FloatArray2d)                 
                    err = nf90_put_var(SELF%ncid,varid,pp%d,start=beg,count=count)
                TYPE IS (FloatArray3d)                   
                    err = nf90_put_var(SELF%ncid,varid,pp%d,start=beg,count=count)
                TYPE IS (FloatArray4d)                  
                    err = nf90_put_var(SELF%ncid,varid,pp%d,start=beg,count=count)
            END SELECT

            IF (ASSOCIATED(pp%onDemand)) CALL pp%free_memory()            

            pp => NULL()
            DEALLOCATE(beg,count)

        END DO

    END SUBROUTINE write_var



    ! PRIVATE routines
    ! ------------------------------------------------------------

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

    ! ------------------------------------------------------------
    ! findRecordIdx: Find the index for the record dimension
    !                in the dimension vector for given variable
    !
    FUNCTION findRecordIdx(SELF,varid)
        !!! OLISKO VAAN HELPOMPI TALLENTAA NIMI JA ETTIÄ SILLÄ SUORAAN
        !!! FIELDARRAY INSTANSSISTA?
        CLASS(OStreamNCDF), INTENT(in) :: SELF
        INTEGER, INTENT(in) :: varid
        INTEGER, ALLOCATABLE :: dimids(:)  ! just allocate enough space to be sure to cover all possibilities... 
        INTEGER :: err,i,ndims
        INTEGER :: findRecordIdx      
        err = nf90_inquire_variable(SELF%ncid,varid,ndims=ndims)
        ALLOCATE(dimids(ndims+1)) ! +1 for error handling
        err = nf90_inquire_variable(SELF%ncid,varid,dimids=dimids)
        findRecordIdx = -1
        DO i = 1,ndims+1
            findRecordIdx = i
            IF (dimids(i) == SELF%recdid) EXIT 
        END DO
        IF (findRecordIdx == ndims+1) findRecordIdx = -1 ! Record dimension not found 
        DEALLOCATE(dimids)   
    END FUNCTION findRecordIdx

    ! ------------------------------------------------------------
    ! Update the variable offset to the current record.
    !
    SUBROUTINE updateOffset(SELF,beg,varid)
        CLASS(OStreamNCDF) :: SELF 
        INTEGER, INTENT(inout) :: beg(:)
        INTEGER, INTENT(in) :: varid  
        INTEGER :: recidx 
        recidx = SELF%findRecordIdx(varid)
        IF (recidx /= -1) beg(recidx) = SELF%nrec
    END SUBROUTINE updateOffset 

    ! -------------------------------------------------------------
    ! excludeRecord: get count or start index arrays excluding
    ! the record dimension. Arr will be the original array.
    !
    FUNCTION excludeRecord(SELF,arr,varid)
        CLASS(OStreamNCDF) :: SELF
        INTEGER, INTENT(in) :: arr(:)
        INTEGER, INTENT(in) :: varid
        INTEGER, ALLOCATABLE :: excludeRecord(:)
        LOGICAL, ALLOCATABLE :: mask(:)
        INTEGER :: recidx
        recidx = SELF%findRecordIdx(varid)
        ALLOCATE(mask(SIZE(arr))); mask = .TRUE.
        IF (recidx /= -1) mask(recidx) = .FALSE.
        ALLOCATE(excludeRecord,SOURCE=PACK(arr,mask))
        DEALLOCATE(mask)
    END FUNCTION excludeRecord


END MODULE classOStreamNCDF