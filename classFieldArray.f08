MODULE classFieldArray
   !
   !
   USE classArrayElement, ONLY : ArrayElement
   USE mo_structured_datatypes

   IMPLICIT NONE

   PRIVATE 
   PUBLIC FieldArray

   INTEGER, PARAMETER :: undefined = 999 

   TYPE FieldArray
      TYPE(ArrayElement), ALLOCATABLE  :: list(:)     ! Each element holds data and attributes to one variable given by the class ArrayElement
      INTEGER                          :: count       = 0       ! Number of entries, initialized as 0
      LOGICAL                          :: Initialized = .FALSE. ! Initialized as .FALSE. will be TRUE after the first entry is made

      CONTAINS

         PROCEDURE :: Extend_FieldArray
         GENERIC   :: Extend => Extend_FieldArray

         PROCEDURE :: ExN_FieldArray         ! WHY IS THIS HERE??
         GENERIC   :: ExN => ExN_FieldArray

         PROCEDURE :: NewField

         PROCEDURE :: getField

         PROCEDURE :: getFieldIndex
         
         PROCEDURE :: getByGroup
         PROCEDURE :: getByOutputstatus
         

         PROCEDURE :: getData_0d, getData_1d, getData_2d, getData_3d, getData_4d
         GENERIC   :: getData => getData_0d, getData_1d, getData_2d, getData_3d, getData_4d         

         PROCEDURE :: getVarInst

         PROCEDURE :: getDimension

         PROCEDURE :: getUnit

         PROCEDURE :: getCountsLocal

         PROCEDURE :: getCountsGlobal

         PROCEDURE :: getOffsets

         PROCEDURE :: getName

         PROCEDURE :: getLongName

         PROCEDURE :: convertToIndex

         PROCEDURE :: Exist
       
         PROCEDURE :: destroy_FieldArray
         GENERIC   :: destroy => destroy_FieldArray


   END TYPE FieldArray
   !------------------------
   INTERFACE FieldArray
      PROCEDURE :: FieldArray_constructor
   END INTERFACE FieldArray

   CONTAINS


   FUNCTION FieldArray_constructor()
      !
      ! ---------------------------------------------------------------
      ! Initialize a FieldArray instance: just puts count to zero and
      ! sets up some switches with initial values
      !
      IMPLICIT NONE
      TYPE(FieldArray) :: FieldArray_constructor

      FieldArray_constructor%count = 0
      FieldArray_constructor%Initialized = .FALSE.
    
   END FUNCTION FieldArray_constructor
  
   !
   ! --------------------------------------------
   ! PROCEDURES BOUND TO FieldArray
   !
   SUBROUTINE newField(SELF,name,dimension,counts_local,counts_global,   &
                       axis_offsets,p_data,t_data,c_data,outputstatus,long_name,unit,group)
      !
      ! ------------------------------------------------------------
      ! Create a new variable in the FieldArray list
      !
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(inout)         :: SELF
      CHARACTER(len=*), INTENT(in)             :: name     ! Variable name
      CHARACTER(len=*), INTENT(in)             :: dimension(:)     ! String that gives the dimension environment for output
      INTEGER, INTENT(in)                      :: counts_global(:)
      INTEGER, INTENT(in)                      :: counts_local(:)
      INTEGER, INTENT(in)                      :: axis_offsets(:)
      CLASS(FloatArray), INTENT(in), POINTER            :: p_data   ! Polymorphic pointer to data (values)
      CLASS(FloatArray), INTENT(in), POINTER, OPTIONAL  :: t_data   ! - '' - (tendencies)
      CLASS(FloatArray), INTENT(in), POINTER, OPTIONAL  :: c_data   ! - '' - (current)
      LOGICAL, INTENT(in), OPTIONAL            :: outputstatus  ! Default FALSE
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: long_name     ! Long name, mainly for output attributes, default empty
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: unit          ! Unit of the variable, e.g. "kg/kg"; used mainly for output attributes, default empty
      CHARACTER(len=*), INTENT(in), OPTIONAL   :: group(:)
 
      ! Extend the variable list allocation in FieldArray
      CALL SELF%Extend()

      ! Pass the input data and parameters to ArrayElement constructor
      SELF%list(SELF%count) = ArrayElement(name,long_name,unit,dimension,counts_local,counts_global,  &
                                           axis_offsets,outputstatus,p_data,t_data,c_data,group)

   END SUBROUTINE newField

   ! ------------------------------------------------------------

   SUBROUTINE Extend_FieldArray(SELF)
      !
      ! ----------------------------------------------------------
      ! Extend the memory allocation of the "list" in FieldArray.
      ! Mainly intended to be used by newField procedure
      !
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(inout) :: SELF

      TYPE(ArrayElement), ALLOCATABLE :: tmp(:)

      IF (SELF%Initialized) THEN

         ALLOCATE(tmp(SELF%count+1))
         tmp(1:SELF%count) = SELF%list
         DEALLOCATE(SELF%list)
         CALL MOVE_ALLOC(tmp,SELF%list)
       
      ELSE
         ALLOCATE(SELF%list(SELF%count+1))
      END IF

      SELF%count = SELF%count + 1
      SELF%Initialized = .TRUE.

   END SUBROUTINE Extend_FieldArray

   ! -------------------------------------------------------------

   SUBROUTINE ExN_FieldArray(SELF,n)
      !
      ! ----------------------------------------------------------
      ! Extend the memory allocation of the "list" in FieldArray.
      ! Mainly intended to be used by newField procedure
      !
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(inout) :: SELF
      INTEGER, INTENT(in) :: n

      TYPE(ArrayElement), ALLOCATABLE :: tmp(:)

      IF (SELF%Initialized) THEN

         ALLOCATE(tmp(SELF%count+n))
         tmp(1:SELF%count) = SELF%list
         DEALLOCATE(SELF%list)
         CALL MOVE_ALLOC(tmp,SELF%list)

      ELSE
         ALLOCATE(SELF%list(SELF%count+n))
      END IF

      SELF%count = SELF%count + n
      SELF%Initialized = .TRUE.

   END SUBROUTINE ExN_FieldArray


   !
   ! ---------------------------------------------
   ! Returns the ArrayElement instance for given
   ! index "ind" of the FieldArray list.
   !
   SUBROUTINE getField(SELF,out,ind,name)

      IMPLICIT NONE
      CLASS(FieldArray), TARGET, INTENT(in) :: SELF
      INTEGER, INTENT(in), OPTIONAL :: ind
      CHARACTER(len=*), INTENT(in), OPTIONAL :: name
      TYPE(ArrayElement), INTENT(out), POINTER :: out
      INTEGER :: lind
      lind=0
      IF (PRESENT(ind)) THEN
         lind = ind
      ELSE IF (PRESENT(name)) THEN
         lind = SELF%getFieldIndex(name)
      END IF
      IF (lind==0) &
           WRITE(*,*) "classFieldArray:getField -- WARNING: Variable not found ",name
      out => SELF%list(lind)

   END SUBROUTINE getField

   ! -----------------------------------------------------------------------------
   ! getGroup returns a FieldArray instance, which contains a subset of variables
   ! from the parent FieldArray instance that belong to the inquired group. Whether
   ! any variables meeting the criterion are found can be checked by the field
   ! FieldArray%Initialized.
   ! 
   SUBROUTINE getByGroup(SELF,groupname,FAout)
     IMPLICIT NONE
     CLASS(FieldArray), INTENT(in)  :: SELF
     CHARACTER(len=*), INTENT(in)   :: groupname
     TYPE(FieldArray), INTENT(out) :: FAout

     LOGICAL :: groupmask(SELF%count)
     INTEGER :: i
     
     FAout = FieldArray()
     
     groupmask = .FALSE.
     DO i = 1,SELF%count
        groupmask(i) = (ANY(SELF%list(i)%group(:) == groupname))
     END DO
        
     FAout%count = COUNT(groupmask)
     IF (FAout%count > 0) THEN
        ALLOCATE(FAout%list(FAout%count))
        FAout%list(:) = PACK(SELF%list(:),groupmask)     
        FAout%Initialized = .TRUE.
     END IF
        
   END SUBROUTINE getByGroup

   SUBROUTINE getByOutputstatus(SELF,FAout)
     IMPLICIT NONE
     CLASS(FieldArray), INTENT(in) :: SELF
     TYPE(FieldArray), INTENT(out) :: FAout

     LOGICAL :: mask(SELF%count)
    
     FAout = FieldArray()

     IF (.NOT. SELF%Initialized) RETURN
     
     mask = .FALSE.
     mask(:) = (SELF%list(:)%outputstatus)

     FAout%count = COUNT(mask)
     IF (FAout%count > 0) THEN
        ALLOCATE(FAout%list(FAout%count))
        FAout%list(:) = PACK(SELF%list(:),mask)     
        FAout%Initialized = .TRUE.
     END IF
        
   END SUBROUTINE getByOutputstatus
   


   ! ---------------------------------------------------------------

   !
   ! --------------------------------------------------------------------
   ! getData provides the main user interface for getting the numerical
   ! data (value - tend=1; tendency - tend=2) for a variable corresponding
   ! to the given index or variable name. getData uses the getVarInst
   ! procedure and then essentially collapses the fetched polymorphic
   ! instance to the correct datatype inferred by the output variable "out".
   ! If the datatype of "out" does not correspond to existing variables
   ! given by "index" or "name", an unassociated pointer with error status
   ! is returned. getData currently overloads the procedures getData_1d,
   ! getData_2d, getData_3d and getData_4d bound to FieldArray.
   !
   SUBROUTINE getData_0d(SELF,out,tlev,key)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(in)            :: SELF
      TYPE(FloatArray0d), INTENT(out), POINTER :: out    ! Data container instance
      CLASS(*), INTENT(in)                     :: key    ! Variable name (CHARACTER) or list index (INTEGER)
      INTEGER, INTENT(in)                      :: tlev   ! Get value (1; default) or tendency (2) or current (for vectors; 3)

      CLASS(FloatArray), POINTER :: pp  ! Polymorphic pointer to the variable instance in ArrayElement

      out => NULL()
      
      CALL SELF%getVarInst(pp,key,tlev)

      ! Collapse to inquired datatype. If the in_name or ind does not point to
      ! variable of this datatype, out will remain unassociated
      IF (ASSOCIATED(pp)) THEN
         SELECT TYPE(pp)
         TYPE IS (FloatArray0d)
            out => pp
         END SELECT
      END IF

   END SUBROUTINE getData_0d
   
   ! ---------
   ! ---------
   SUBROUTINE getData_1d(SELF,out,tlev,key)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(in)            :: SELF
      TYPE(FloatArray1d), INTENT(out), POINTER :: out    ! Data container instance
      CLASS(*), INTENT(in)                     :: key    ! Variable name (CHARACTER) or list index (INTEGER)
      INTEGER, INTENT(in)                      :: tlev   ! Get value (1; default) or tendency (2) or current (for vectors; 3)

      CLASS(FloatArray), POINTER :: pp  ! Polymorphic pointer to the variable instance in ArrayElement

      out => NULL()

      CALL SELF%getVarInst(pp,key,tlev)

      ! Collapse to inquired datatype. If the in_name or ind does not point to
      ! variable of this datatype, out will remain unassociated
      IF (ASSOCIATED(pp)) THEN
         SELECT TYPE(pp)
         TYPE IS (FloatArray1d)
            out => pp
         END SELECT
      END IF
         
   END SUBROUTINE getData_1d
   ! ---------
   ! ---------
   SUBROUTINE getData_2d(SELF,out,tlev,key)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(in)            :: SELF
      TYPE(FloatArray2d), INTENT(out), POINTER :: out    ! Data container instance
      CLASS(*), INTENT(in)                     :: key    ! Variable name (CHARACTER) or list index (INTEGER)
      INTEGER, INTENT(in)                      :: tlev   ! Get value (1; default) or tendency (2) or current (for vectors; 3)

      CLASS(FloatArray), POINTER :: pp  ! Polymorphic pointer to the variable instance in ArrayElement

      out => NULL()
   
      CALL SELF%getVarInst(pp,key,tlev)

      ! Collapse to inquired datatype. If the in_name or ind does not point to
      ! variable of this datatype, out will remain unassociated
      IF (ASSOCIATED(pp)) THEN
         SELECT TYPE(pp)
         TYPE IS (FloatArray2d)
            out => pp
         END SELECT
      END IF
         
   END SUBROUTINE getData_2d
   ! ---------
   ! ---------
   SUBROUTINE getData_3d(SELF,out,tlev,key)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(in)            :: SELF
      TYPE(FloatArray3d), INTENT(out), POINTER :: out    ! Data container instance
      CLASS(*), INTENT(in)                     :: key    ! Variable name (CHARACTER) or list index (INTEGER) 
      INTEGER, INTENT(in)                      :: tlev   ! Get value (1; default) or tendency (2) or current (for vectors; 3)

      CLASS(FloatArray), POINTER :: pp  ! Polymorphic pointer to the variable instance in ArrayElement

      out => NULL()
      
      CALL SELF%getVarInst(pp,key,tlev)

      ! Collapse to inquired datatype. If the in_name or ind does not point to
      ! variable of this datatype, out will remain unassociated
      IF (ASSOCIATED(pp)) THEN
         SELECT TYPE(pp)
         TYPE IS (FloatArray3d)
            out => pp
         END SELECT
      END IF
         
   END SUBROUTINE getData_3d
   ! ---------
   ! ---------
   SUBROUTINE getData_4d(SELF,out,tlev,key)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(in)            :: SELF
      TYPE(FloatArray4d), INTENT(out), POINTER :: out    ! Data container instance
      CLASS(*), INTENT(in)                     :: key    ! Variable name (CHARACTER) or list index (INTEGER) 
      INTEGER, INTENT(in)                      :: tlev   ! Get value (1; default) or tendency (2) or current (for vectors; 3)

      CLASS(FloatArray), POINTER :: pp  ! Polymorphic pointer to the variable instance in ArrayElement

      out => NULL()
      
      CALL SELF%getVarInst(pp,key,tlev)

      ! Collapse to inquired datatype. If the in_name or ind does not point to
      ! variable of this datatype, out will remain unassociated
      IF (ASSOCIATED(pp)) THEN
         SELECT TYPE(pp)
         TYPE IS (FloatArray4d)
            out => pp
         END SELECT
      END IF
         
   END SUBROUTINE getData_4d

   ! -----------------------------------------------------------------
   !
   ! ----------------------------------------------------------------------------
   ! getVarInst returns the polymorphic pointer to the FloatArray instances p or t (or c)
   ! within the class ArrayElement depending on tlev. The variable is selected by key, 
   ! which can be the FieldArray%list index (INTEGER) or variable name (CHARACTER)


   SUBROUTINE getVarInst(SELF,out,key,tlev)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      INTEGER, INTENT(in) :: tlev
      CLASS(FloatArray), POINTER, INTENT(out) :: out
      TYPE(ArrayElement), POINTER :: Element
      INTEGER :: ind

      out => NULL()
      Element => NULL()
      ind = SELF%convertToIndex(key)

      IF (ind <= SELF%count .AND. ind >= 1 .AND. ind /= undefined) & ! If index is bad, Element and out will remain unassociated
         CALL SELF%getField(Element,ind=ind)      

      IF (tlev==1 .AND. ASSOCIATED(Element)) THEN
         CALL Element%get_p(out)
      ELSE IF (tlev==2 .AND. ASSOCIATED(Element)) THEN
         CALL Element%get_t(out)
      ELSE IF (tlev==3 .AND. ASSOCIATED(Element)) THEN
         CALL Element%get_c(out)
      END IF

      Element => NULL()

   END SUBROUTINE getVarInst

   ! -----------------------------------------------------------------

   FUNCTION getDimension(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      CHARACTER(len=:), ALLOCATABLE :: getDimension(:)
      INTEGER :: ind
      ind = SELF%convertToIndex(key)
      getDimension = SELF%list(ind)%getDimension()
   END FUNCTION getDimension

   ! -----------------------------------------------------------------
   
   FUNCTION getUnit(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      CHARACTER(len=:), ALLOCATABLE :: getUnit
      INTEGER :: ind
      ind = SELF%convertToIndex(key)
      getUnit = SELF%list(ind)%getUnit()
   END FUNCTION getUnit

   ! -----------------------------------------------------------------

   FUNCTION getName(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      CHARACTER(len=:), ALLOCATABLE :: getName
      INTEGER :: ind
      ind = SELF%convertToIndex(key)
      getName = SELF%list(ind)%getName()
   END FUNCTION getName

   ! -----------------------------------------------------------------

   FUNCTION getLongName(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      CHARACTER(len=:), ALLOCATABLE :: getLongName
      INTEGER :: ind
      ind = SELF%convertToIndex(key)
      getLongName = SELF%list(ind)%getLongName()
   END FUNCTION getLongName

   ! -----------------------------------------------------------------

   FUNCTION getCountsLocal(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      INTEGER, ALLOCATABLE :: getCountsLocal(:)
      INTEGER :: ind
      ind = SELF%convertToIndex(key)
      getCountsLocal = SELF%list(ind)%getCountsLocal()
   END FUNCTION getCountsLocal

   ! -----------------------------------------------------------------

   FUNCTION getCountsGlobal(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      INTEGER, ALLOCATABLE :: getCountsGlobal(:)
      INTEGER :: ind 
      ind = SELF%convertToIndex(key)
      getCountsGlobal = SELF%list(ind)%getCountsGlobal()
   END FUNCTION getCountsGlobal

   ! -----------------------------------------------------------------

   FUNCTION getOffsets(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      INTEGER, ALLOCATABLE :: getOffsets(:)
      INTEGER :: ind 
      ind = SELF%convertToIndex(key)
      getOffsets = SELF%list(ind)%getOffsets()
   END FUNCTION getOffsets

   ! -----------------------------------------------------------------

   FUNCTION convertToIndex(SELF,key)
      CLASS(FieldArray), INTENT(in) :: SELF
      CLASS(*), INTENT(in) :: key
      INTEGER :: convertToIndex
      SELECT TYPE(key)
         TYPE IS (INTEGER)
            convertToIndex = key
         TYPE IS (CHARACTER(len=*))
            convertToIndex = 0
            IF ( SELF%Exist(key) ) THEN
               convertToIndex = SELF%getFieldIndex(key)
            END IF            
      END SELECT

   END FUNCTION convertToIndex


   !
   ! ---------------------------------------------------------
   ! Returns the FieldArray index with name=in_name
   !
   INTEGER FUNCTION getFieldIndex(SELF,in_name)
      CLASS(FieldArray), INTENT(in) :: SELF
      CHARACTER(len=*), INTENT(in)  :: in_name
      INTEGER :: i

      i = 1
      DO
         IF (i > SELF%count) THEN
            i = undefined
            EXIT
         END IF
         IF (SELF%list(i)%name == in_name) THEN
            EXIT
         END IF
         i = i + 1
      END DO
      getFieldIndex = i
      
   END FUNCTION getFieldIndex


   ! -----------------------------------------------------------------

    LOGICAL FUNCTION Exist(SELF,iname)
     CLASS(FieldArray), INTENT(in) :: SELF
     CHARACTER(len=*), INTENT(in) :: iname

     INTEGER :: i
     
     ! Check if variable with iname exists
     Exist = .FALSE.
     DO i = 1,SELF%count
        IF (SELF%list(i)%name == iname) THEN
           Exist = .TRUE.
           EXIT
        END IF
     END DO     
   END FUNCTION Exist
   
   
   ! ---------------------------------------------------------------

   SUBROUTINE destroy_FieldArray(SELF)
      IMPLICIT NONE
      CLASS(FieldArray), INTENT(inout) :: SELF
    
      DEALLOCATE(SELF%list)

   END SUBROUTINE destroy_FieldArray
   

END MODULE classFieldArray
