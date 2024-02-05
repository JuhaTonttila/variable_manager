MODULE classOStreamHDF5
    USE hdf5
    USE h5ds
    USE mo_parameters, ONLY : ch_short,ch_long
    !USE h5i
    !USE h5lt
    IMPLICIT NONE 

    INTEGER, PARAMETER :: nds = 5 ! Max number of dataspace instances 
    INTEGER, PARAMETER :: undef = -1


    TYPE OStreamHDF5

        INTEGER(HID_T) :: h5id = 0
        CHARACTER(len=ch_long) :: fname ! Output filename for the stream
        INTEGER :: nrec
        CHARACTER(len=ch_short) :: drecname  ! record dimension name. For now assume there's just one, could extend later...
        !INTEGER(HID_T) :: dspace(nds) = undef  ! dataspace instaces from 1 to 5d by index (including time)

        CONTAINS

        PROCEDURE :: open_h5, close_h5,   &
                     define_dimensions,   & 
                     define_variables,    &
                     write_var,           &
                     newRecord
                     !close_dataspaces
    
        PROCEDURE, PRIVATE :: findRecordIdx,    &
                              excludeRecord,    &
                              updateOffset


    END TYPE OStreamHDF5
    INTERFACE OStreamHDF5 
        PROCEDURE constructor
    END INTERFACE OStreamHDF5     

    CONTAINS 

    ! ---------------------------------

    FUNCTION constructor(fname)
        CHARACTER(len=*), INTENT(in) :: fname
        TYPE(OStreamHDF5) :: constructor
        constructor%fname = fname
    END FUNCTION constructor

    ! ---------------------------------

    SUBROUTINE open_h5(SELF)
        CLASS(OStreamHDF5), INTENT(inout) :: SELF
        INTEGER :: err 
        INTEGER(HID_T) filespace
        ! Initialize HDF5 library
        CALL h5open_f(err)
        ! Create file access property list
        CALL h5pcreate_f(H5P_FILE_ACCESS_F, filespace, err )
        ! Create a new output file (CHECK THE FILE OPEN METHOD)
        CALL h5fcreate_f(SELF%fname, H5F_ACC_TRUNC_F, SELF%h5id, err, access_prp=filespace )
        ! Close the file property list instance
        CALL h5pclose_f(filespace, err)
        SELF%nrec = 0  !! This has to have some additional stuff for opening pre-existing files
    END SUBROUTINE open_h5

    ! -----------------------------------

    SUBROUTINE define_dimensions(SELF,dims,recGrp)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamHDF5), INTENT(inout) :: SELF
        TYPE(FieldArray), INTENT(inout) :: dims
        CHARACTER(len=*), INTENT(in), OPTIONAL :: recGrp

        CHARACTER(len=10) :: recg_ 
        INTEGER :: n, i, err 
        INTEGER(HID_T), ALLOCATABLE :: lcounts_(:), gcounts_(:)
        INTEGER(HID_T) :: dataspace,did,dcpl

        recg_ = ""
        IF (PRESENT(recGrp)) recg_ = recGrp

        n = dims%count
        DO i = 1,n

            lcounts_ = INT(dims%getCountsLocal(i),KIND=HSIZE_T)
            gcounts_ = INT(dims%getCountsGlobal(i),KIND=SIZE_T)
            CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,err); 
            CALL H5Pset_chunk_f(dcpl, 1, lcounts_, err);
            ! Create dimension
            IF (dims%isInGroup(recg_,i)) THEN
                CALL h5screate_simple_f(1, gcounts_-1, dataspace, err,   &
                                        maxdims = [H5S_UNLIMITED_F])
                SELF%drecname = dims%getName(i)
            ELSE  
                CALL h5screate_simple_f(1, gcounts_, dataspace, err)
            END IF 
            CALL h5dcreate_f(SELF%h5id, dims%getName(i), H5T_NATIVE_REAL, &
                             dataspace, did, err, dcpl_id = dcpl)

            ! make into dimension scale
            CALL h5dsset_scale_f(did,err,dims%getName(i))

            CALL h5sclose_f(dataspace,err)
            CALL h5dclose_f(did,err)
            DEALLOCATE(gcounts_,lcounts_)

        END DO

    END SUBROUTINE define_dimensions

    ! --------------------------------------------------

    SUBROUTINE define_variables(SELF,vars)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamHDF5), INTENT(inout) :: SELF
        TYPE(FieldArray), INTENT(inout) :: vars
        INTEGER :: i,n,err,ridx
        INTEGER(HID_T), ALLOCATABLE :: gcounts_(:), lcounts_(:), maxdims_(:)
        INTEGER(HID_T) :: did, dcpl, dspace

        ! Check if vars%count > 0?
        ! Should also take care of the case where continuing pre-existing file

        DO i = 1,vars%count
            gcounts_ = INT(vars%getCountsGlobal(i),KIND=HSIZE_T)
            lcounts_ = INT(vars%getCountsLocal(i),KIND=HSIZE_T)
            n = SIZE(gcounts_)

            CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,err);
            CALL H5Pset_chunk_f(dcpl, n, lcounts_, err);

            ridx = SELF%findRecordIdx(vars%getDimension(i))
            maxdims_ = gcounts_ 
            IF (ridx > 0) THEN 
                maxdims_(ridx) = H5S_UNLIMITED_F
                gcounts_(ridx) = gcounts_(ridx) - 1
            END IF

            CALL h5screate_simple_f(n, gcounts_, dspace, err, maxdims = maxdims_) 
                                                             
            CALL h5dcreate_f(SELF%h5id, vars%getName(i), H5T_NATIVE_REAL, &
                             dspace, did, err, dcpl_id = dcpl)

            CALL h5dclose_f(did,err)
            CALL h5sclose_f(dspace,err)
            DEALLOCATE(gcounts_,lcounts_,maxdims_)
        END DO

    END SUBROUTINE define_variables

    ! --------------------------------------------------

    SUBROUTINE newRecord(SELF,record)
        USE classFieldArray, ONLY : FieldArray
        CLASS(OStreamHDF5) :: SELF
        TYPE(FieldArray), INTENT(inout) :: record
        SELF%nrec = SELF%nrec + 1 
        CALL SELF%write_var(record)        
    END SUBROUTINE newRecord

    ! --------------------------------------------------

    SUBROUTINE write_var(SELF,vars)   !!! Come up with better name...
        USE classFieldArray, ONLY : FieldArray
        USE mo_structured_datatypes
        CLASS(OStreamHDF5), INTENT(in) :: SELF
        TYPE(FieldArray), INTENT(inout) :: vars

        CLASS(FloatArray), POINTER :: pp

        INTEGER :: err, n, i, nd 
        INTEGER(HID_T) :: varid, dspace, mspace
        INTEGER(HSIZE_T), ALLOCATABLE :: lcounts(:), offsets(:)
        INTEGER(HSIZE_T), ALLOCATABLE :: lc_exrec(:) ! Local counts excluding record dimension

        pp => NULL()
        n = vars%count

        DO i = 1,n

            CALL vars%getVarInst(pp, i, 1)
            lcounts = INT(vars%getCountsLocal(i),KIND=HSIZE_T)
            lc_exrec = SELF%excludeRecord(lcounts,vars%getDimension(i))
            offsets = INT(vars%getOffsets(i),KIND=HSIZE_T)

            WRITE(*,*) 'HEP'
            CALL h5dopen_f(SELF%h5id,vars%getName(i),varid,err)
            CALL h5dget_space_f(varid,dspace,err)
            WRITE(*,*) 'HEP2'

            IF (ASSOCIATED(pp%onDemand)) THEN
                CALL pp%allocate_internal(INT(lc_exrec))  
                CALL pp%onDemand()
            END IF
            CALL SELF%updateOffset(offsets,vars%getDimension(i))
            !! SET_OFFSET to update the variable instance??

            CALL h5sselect_hyperslab_f(dspace,H5S_SELECT_SET_F,offsets,lcounts,err)

            nd = SIZE(lcounts)
            CALL h5screate_simple_f(nd,lcounts,mspace,err)

            SELECT TYPE(pp)
            TYPE IS(FloatArray0d)
                CALL h5dwrite_f( varid,H5T_NATIVE_REAL,pp%d,lc_exrec,err,   &
                                 file_space_id=dspace,mem_space_id=mspace   )
            TYPE IS(FloatArray1d)
                CALL h5dwrite_f( varid,H5T_NATIVE_REAL,pp%d,lc_exrec,err,   &
                                 file_space_id=dspace,mem_space_id=mspace   )
            TYPE IS(FloatArray2d)
                CALL h5dwrite_f( varid,H5T_NATIVE_REAL,pp%d,lc_exrec,err,   &
                                 file_space_id=dspace,mem_space_id=mspace   )
            TYPE IS(FloatArray3d)
                CALL h5dwrite_f( varid,H5T_NATIVE_REAL,pp%d,lc_exrec,err,   &
                                 file_space_id=dspace,mem_space_id=mspace   )
            TYPE IS(FloatArray4d)
                CALL h5dwrite_f( varid,H5T_NATIVE_REAL,pp%d,lc_exrec,err,   &
                                 file_space_id=dspace,mem_space_id=mspace   )
            END SELECT            

            IF (ASSOCIATED(pp%onDemand)) CALL pp%free_memory()            
            pp => NULL()
            DEALLOCATE(offsets,lcounts,lc_exrec)
            CALL h5sclose_f(dspace,err) 
            CALL h5sclose_f(mspace,err)

        END DO

    END SUBROUTINE write_var 



    ! --------------------------------------------------

    !SUBROUTINE close_dataspaces(SELF)
    !    CLASS(OStreamHDF5), INTENT(inout) :: SELF
    !    INTEGER :: i,err  
    !    ! Close all dataspaces
    !    DO i = 1,nds 
    !        IF (SELF%dspace(i) /= undef) THEN 
    !            CALL h5sclose_f(SELF%dspace(i),err)
    !            SELF%dspace(i) = undef
    !        END IF
    !    END DO 

    !END SUBROUTINE close_dataspaces 

    ! --------------------------------------------------

    SUBROUTINE close_h5(SELF)
        CLASS(OStreamHDF5), INTENT(inout) :: SELF
        INTEGER :: err 
        CALL h5fclose_f(SELF%h5id,err)
        CALL h5close_f(err)
    END SUBROUTINE close_h5 


    ! ------------------------------------------------------

    FUNCTION findRecordIdx(SELF,dims)
        CLASS(OStreamHDF5), INTENT(in) :: SELF
        CHARACTER(len=*), INTENT(in) :: dims(:) ! List of dimensions from where to search
        INTEGER :: i 
        INTEGER :: findRecordIdx 

        findRecordIdx = undef
        DO i = 1, SIZE(dims)
            IF (dims(i) == SELF%drecname) THEN
                findRecordIdx = i 
                EXIT
            END IF
        END DO 

    END FUNCTION findRecordIdx

    ! ------------------------------------------------------

    FUNCTION excludeRecord(SELF,counts_,dims_)
        ! Get the dimension lengths (count_) excluding the record dimension. 
        ! For now assumes only 1 record dim
        CLASS(OStreamHDF5), INTENT(in) :: SELF
        INTEGER(HSIZE_T), INTENT(in) :: counts_(:)
        CHARACTER(len=*), INTENT(in) :: dims_(:)
        INTEGER(HSIZE_T), ALLOCATABLE :: excludeRecord(:)
        LOGICAL, ALLOCATABLE :: mask(:)
        INTEGER :: ridx
        ridx = SELF%findRecordIdx(dims_)
        ALLOCATE(mask(SIZE(counts_))); mask = .TRUE.
        IF (ridx /= undef) mask(ridx) = .FALSE.
        ALLOCATE(excludeRecord,SOURCE=PACK(counts_,mask))
        DEALLOCATE(mask)
    END FUNCTION excludeRecord

    ! ------------------------------------------------------------
    ! Update the variable offset to the current record.
    !
    SUBROUTINE updateOffset(SELF,offsets_,dims_)
        CLASS(OStreamHDF5) :: SELF 
        INTEGER(HSIZE_T), INTENT(inout) :: offsets_(:)
        CHARACTER(len=*), INTENT(in) :: dims_(:)
        INTEGER :: ridx 
        ridx = SELF%findRecordIdx(dims_)
        IF (ridx /= undef) offsets_(ridx) = SELF%nrec
    END SUBROUTINE updateOffset 


END MODULE classOStreamHDF5