MODULE classArrayElement
    USE mo_structured_datatypes, ONLY : FloatArray
    USE mo_parameters, ONLY : ch_long, ch_short
    IMPLICIT NONE

    PRIVATE
    PUBLIC ArrayElement


    TYPE ArrayElement
        CHARACTER(len=ch_short)  :: name          ! Short name: for output variable name and for fetching data using getData
        CHARACTER(len=ch_long) :: long_name     ! Long name, mainly for output attributes
        CHARACTER(len=ch_short) :: unit          ! Unit of the variable, e.g. "kg/kg"; used mainly for output attributes
        CHARACTER(len=ch_Short), ALLOCATABLE  :: dimension(:)  ! list of dimension names associated with this variable. The dimensions will be allocated in this
                                                     ! order to the output files. The dimensions must be defined in mo_aux_state (CHECK)
        INTEGER, ALLOCATABLE :: counts_local(:)           ! Length of the axes for this variable for the local PE, must correspond to %dimension 
        INTEGER, ALLOCATABLE :: counts_global(:)          ! Global (across PEs) axis lengths
        INTEGER, ALLOCATABLE :: axis_offsets(:)           ! Offset for each dimension for local PE in the global grid

        CHARACTER(len=ch_short), ALLOCATABLE  :: group(:)         ! A group tag that can be used to fetch a list of certain type variables
        LOGICAL            :: outputstatus  ! TRUE: write this variable to an output file. FALSE: don't.

        ! Below are pointers to scalar variable arrays. In U-S, scalars have two definitions:
        ! the previous value (p) and the tendency (t). For vector variables the is also the
        ! current (c) state. These are given here as unlimited
        ! polymorphic pointers. This makes it possible to associate them with any Fortran intrinsic
        ! or derived data type, without duplicating the code. However, for arrays of different ranks,
        ! derived datatypes must be used, which are available for 1-4d arrays (easy to add more...)
        ! in mo_sturctured_datatypes.f90

        ! For diagnostic variables, where tendency is not needed, just use "p". Providing
        ! the tendency arrays e.g. for the constructor routine is optional.

        CLASS(FloatArray), POINTER :: p => NULL()
        CLASS(FloatArray), POINTER :: t => NULL()
        CLASS(FloatArray), POINTER :: c => NULL()
    
        CONTAINS
            PROCEDURE :: get_p
            PROCEDURE :: get_t
            PROCEDURE :: get_c

            PROCEDURE :: getName
            PROCEDURE :: getLongName
            PROCEDURE :: getUnit
            PROCEDURE :: getDimension
            PROCEDURE :: getCountsLocal
            PROCEDURE :: getCountsGlobal
            PROCEDURE :: getOffsets
            PROCEDURE :: getGroups

    END TYPE ArrayElement

    !-----------------------
    INTERFACE ArrayElement
        PROCEDURE :: cnstr
    END INTERFACE ArrayElement

    CONTAINS
        ! Constructor
        FUNCTION cnstr(name_,long_name_,unit_,dimension_,counts_local_,counts_global_,  &
                       axis_offsets_,outputstatus_,p_data_,t_data_,c_data_,group_      )
            !
            ! --------------------------------
            ! Instantiate a new ArrayElement
            !
            TYPE(ArrayElement)                        :: cnstr
            CHARACTER(len=*), INTENT(in)              :: name_        ! Variable name
            CHARACTER(len=*), INTENT(in)              :: dimension_(:)  ! List of axis dimension shortnames for this variable, including record dimension
            INTEGER, INTENT(in)                       :: counts_local_(:) ! Length of axis dimensions for local PE
            INTEGER, INTENT(in)                       :: counts_global_(:) ! Length of axis dimensions across PEs
            INTEGER, INTENT(in)                       :: axis_offsets_(:)  ! Axis offsets for local PE in the global grid
            CLASS(FloatArray), INTENT(in), POINTER             :: p_data_      ! Polymorphic pointer to data (values)
            CLASS(FloatArray), INTENT(in), POINTER, OPTIONAL   :: t_data_      ! - '' - (tendencies)
            CLASS(FloatArray), INTENT(in), POINTER, OPTIONAL   :: c_data_      ! - '' - (current; for vectors)
            CHARACTER(len=*), INTENT(in), OPTIONAL    :: long_name_     ! Long name, mainly for output attributes
            CHARACTER(len=*), INTENT(in), OPTIONAL    :: unit_          ! Unit of the variable, e.g. "kg/kg"; used mainly for output attributes
            LOGICAL, INTENT(in), OPTIONAL             :: outputstatus_            
            CHARACTER(len=*), INTENT(in), OPTIONAL    :: group_(:)

            cnstr%name = name_
            cnstr%dimension = dimension_   
            cnstr%counts_local = counts_local_
            cnstr%counts_global = counts_global_
            cnstr%axis_offsets = axis_offsets_          
            cnstr%p => p_data_

            IF ( PRESENT(long_name_) ) THEN
                cnstr%long_name = long_name_
            ELSE 
                cnstr%long_name = ""
            END IF

            IF ( PRESENT(unit_) ) THEN
                cnstr%unit = unit_
            ELSE 
                cnstr%unit = ""
            END IF

            IF ( PRESENT(outputstatus_) ) THEN
                cnstr%outputstatus = outputstatus_
            ELSE
                cnstr%outputstatus = .FALSE.
            END IF


            IF (PRESENT(group_)) THEN
                cnstr%group = group_
            ELSE
                ! Set group as "default" if nothing given
                cnstr%group = ['default']
            END IF
            IF (PRESENT(t_data_)) cnstr%t => t_data_
            IF (PRESENT(c_data_)) cnstr%c => c_data_

        END FUNCTION cnstr

        ! --------------------------------------------------------------

        SUBROUTINE get_p(SELF,out)
            CLASS(ArrayElement), INTENT(in) :: SELF
            CLASS(FloatArray), INTENT(out), POINTER  :: out
            out => SELF%p
        END SUBROUTINE get_p
      
        ! --------------------------------------------------------------
      
        SUBROUTINE get_t(SELF,out)
            CLASS(ArrayElement), INTENT(in) :: SELF
            CLASS(FloatArray), INTENT(out), POINTER  :: out
            out => SELF%t
        END SUBROUTINE get_t
      
        ! --------------------------------------------------------------
      
        SUBROUTINE get_c(SELF,out)
            CLASS(ArrayElement), INTENT(in) :: SELF
            CLASS(FloatArray), INTENT(out), POINTER  :: out
            out => SELF%c
        END SUBROUTINE get_c
      
        ! --------------------------------------------------------------

        FUNCTION getName(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            CHARACTER(len=ch_short) :: getName
            getName = SELF%name
        END FUNCTION getName

        ! --------------------------------------------------------------

        FUNCTION getLongName(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            CHARACTER(len=ch_long) :: getLongName
            getLongName = SELF%long_name
        END FUNCTION getLongName

        ! --------------------------------------------------------------

        FUNCTION getUnit(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            CHARACTER(len=ch_short) :: getUnit
            getUnit = SELF%unit
        END FUNCTION getUnit

        ! --------------------------------------------------------------

        FUNCTION getDimension(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            CHARACTER(len=ch_short), ALLOCATABLE :: getDimension(:)
            getDimension = SELF%dimension
        END FUNCTION getDimension

        ! --------------------------------------------------------------

        FUNCTION getCountsLocal(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            INTEGER, ALLOCATABLE :: getCountsLocal(:)
            getCountsLocal = SELF%counts_local
        END FUNCTION getCountsLocal

        ! --------------------------------------------------------------

        FUNCTION getCountsGlobal(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            INTEGER, ALLOCATABLE :: getCountsGlobal(:)
            getCountsGlobal = SELF%counts_global
        END FUNCTION getCountsGlobal

        ! --------------------------------------------------------------

        FUNCTION getOffsets(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF            
            INTEGER, ALLOCATABLE :: getOffsets(:)
            getOffsets = SELF%axis_offsets
        END FUNCTION getOffsets        

        ! --------------------------------------------------------------

        FUNCTION getGroups(SELF)
            CLASS(ArrayElement), INTENT(in) :: SELF 
            CHARACTER(len=ch_short), ALLOCATABLE :: getGroups(:)
            getGroups = SELF%group
        END FUNCTION getGroups 


END MODULE classArrayElement