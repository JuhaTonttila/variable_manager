MODULE mo_structured_datatypes

   IMPLICIT NONE
   

   ! This is the base type for the main datatypes
   TYPE, ABSTRACT :: FloatArray
      CHARACTER(len=50) :: shortName = ''               ! Short name of the variable
      
      CHARACTER(len=50) :: srcName = ''                 ! Holds the short name of the grid-variable used as the
                                                        ! source to calculate e.g. corresponding statistical moments
                                                        ! (mean, variance etc.). 
      LOGICAL :: Initialized = .FALSE.
      LOGICAL :: external_data = .FALSE.                ! Whether the subclass member %d is associated to external storage

      PROCEDURE(odintf), POINTER, PASS :: onDemand => NULL() ! Procedure pointer to a subroutine
                                                             ! for calculating parameters on-demand.

      CONTAINS
         PROCEDURE :: gen_init   ! Member subroutine to initialize the generic parts in the subclass constructors
         PROCEDURE :: free_memory
         PROCEDURE :: allocate_internal
   END TYPE FloatArray
   
   ABSTRACT INTERFACE 
      ! The target procedure for %onDemand must place the results in the object member
      ! %d and take care of the proper memory allocation.
      SUBROUTINE odintf(SELF)
        IMPORT FloatArray
        CLASS(FloatArray), INTENT(inout) :: SELF   ! Variable instance
      END SUBROUTINE odintf
   END INTERFACE

   ! ------------------------------------------
   
   TYPE, EXTENDS(FloatArray) :: FloatArray0d
      REAL, POINTER :: d => NULL() ! Generic access point to the data
   END TYPE FloatArray0d
   INTERFACE FloatArray0d
      PROCEDURE :: cfa0d       ! constructor
   END INTERFACE FloatArray0d
   
   TYPE, EXTENDS(FloatArray) :: FloatArray1d
      REAL, POINTER :: d(:) => NULL() ! Generic access point to the data
   END TYPE FloatArray1d
   INTERFACE FloatArray1d
      PROCEDURE :: cfa1d       ! constructor
   END INTERFACE FloatArray1d

   TYPE, EXTENDS(FloatArray) :: FloatArray2d
      REAL, POINTER :: d(:,:) => NULL() ! Generic access point to the data
   END TYPE FloatArray2d
   INTERFACE FloatArray2d
      PROCEDURE :: cfa2d       ! constructor
   END INTERFACE FloatArray2d

   TYPE, EXTENDS(FloatArray) :: FloatArray3d
      REAL, POINTER :: d(:,:,:) => NULL() ! Generic access point to the data
   END TYPE FloatArray3d
   INTERFACE FloatArray3d
      PROCEDURE :: cfa3d       ! constructor
   END INTERFACE FloatArray3d

   TYPE, EXTENDS(FloatArray) :: FloatArray4d
      REAL, POINTER :: d(:,:,:,:) => NULL() ! Generic access point to the data
   END TYPE FloatArray4d
   INTERFACE FloatArray4d
      PROCEDURE :: cfa4d       ! constructor
   END INTERFACE FloatArray4d




   
   CONTAINS

   ! ----------------------------------------------------
   FUNCTION cfa0d(name,srcname,trgt,func)
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      REAL, INTENT(in), TARGET, OPTIONAL :: trgt
      PROCEDURE(odintf), OPTIONAL :: func
      TYPE(FloatArray0d) :: cfa0d

      ! Initialize the generic parts
      CALL cfa0d%gen_init(name,srcname,func)
     
      ! Associate the data array pointer to external storage if provided
      IF (PRESENT(trgt)) THEN
         cfa0d%d => trgt;
         cfa0d%external_data = .TRUE.
      END IF

      cfa0d%Initialized = .TRUE.
          
   END FUNCTION cfa0d

   ! ----------------------------------------------------
   FUNCTION cfa1d(name,srcname,trgt,func)
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      REAL, INTENT(in), TARGET, OPTIONAL :: trgt(:)
      PROCEDURE(odintf), OPTIONAL :: func
      TYPE(FloatArray1d) :: cfa1d

      ! Initialize the generic parts
      CALL cfa1d%gen_init(name,srcname,func)
     
      ! Associate the data array pointer to external storage if provided
      IF (PRESENT(trgt)) THEN    
         cfa1d%d => trgt(:) 
         cfa1d%external_data = .TRUE.
      END IF

      cfa1d%Initialized = .TRUE.
          
   END FUNCTION cfa1d

   ! ----------------------------------------------------
   FUNCTION cfa2d(name,srcname,trgt,func)
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      REAL, INTENT(in), TARGET, OPTIONAL :: trgt(:,:)
      PROCEDURE(odintf), OPTIONAL :: func
      TYPE(FloatArray2d) :: cfa2d

      ! Initialize the generic parts
      CALL cfa2d%gen_init(name,srcname,func)
     
      ! Associate the data array pointer to external storage if provided
      IF (PRESENT(trgt)) THEN 
         cfa2d%d => trgt(:,:)
         cfa2d%external_data = .TRUE.
      END IF

      cfa2d%Initialized = .TRUE.
          
   END FUNCTION cfa2d

   ! ----------------------------------------------------
   FUNCTION cfa3d(name,srcname,trgt,func)
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      REAL, INTENT(in), TARGET, OPTIONAL :: trgt(:,:,:)
      PROCEDURE(odintf), OPTIONAL :: func
      TYPE(FloatArray3d) :: cfa3d

      ! Initialize the generic parts
      CALL cfa3d%gen_init(name,srcname,func)
     
      ! Associate the data array pointer to external storage if provided
      IF (PRESENT(trgt)) THEN    
         cfa3d%d => trgt(:,:,:) 
         cfa3d%external_data = .TRUE.
      END IF

      cfa3d%Initialized = .TRUE.
          
   END FUNCTION cfa3d

   ! ----------------------------------------------------
   FUNCTION cfa4d(name,srcname,trgt,func)
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      REAL, INTENT(in), TARGET, OPTIONAL :: trgt(:,:,:,:)
      PROCEDURE(odintf), OPTIONAL :: func
      TYPE(FloatArray4d) :: cfa4d

      ! Initialize the generic parts
      CALL cfa4d%gen_init(name,srcname,func)
     
      ! Associate the data array pointer to external storage if provided
      IF (PRESENT(trgt)) THEN    
         cfa4d%d => trgt(:,:,:,:) 
         cfa4d%external_data = .TRUE.
      END IF

      cfa4d%Initialized = .TRUE.
          
   END FUNCTION cfa4d


   SUBROUTINE gen_init(SELF,name,srcname,func)
      CLASS(FloatArray) :: SELF
      CHARACTER(len=*), INTENT(in) :: name
      CHARACTER(len=*), INTENT(in), OPTIONAL :: srcname
      PROCEDURE(odintf), OPTIONAL :: func

      SELF%shortName = name
      IF (PRESENT(srcname)) THEN
         SELF%srcName = srcname
      ELSE
         ! If srcname is not provided, assume that srcName is the same as shortName
         SELF%srcName = name
      END IF

      ! On-demand procedure
      IF (PRESENT(func)) THEN
         SELF%onDemand => func
      ELSE 
         SELF%onDemand => NULL()
      END IF

   END SUBROUTINE gen_init


   SUBROUTINE free_memory(SELF)
      CLASS(FloatArray) :: SELF

      IF (SELF%external_data) THEN
         WRITE(*,*) "%free_memory: warning, class member %d associated with external storage. Ignoring the call"
         RETURN  
      END IF

      ! Not sure if this could be done without the typecasting...
      SELECT TYPE(SELF)
      TYPE IS (FloatArray0d)
         IF (ASSOCIATED(SELF%d)) DEALLOCATE(SELF%d)
      TYPE IS (FloatArray1d)
         IF (ASSOCIATED(SELF%d)) DEALLOCATE(SELF%d)
      TYPE IS (FloatArray2d)
         IF (ASSOCIATED(SELF%d)) DEALLOCATE(SELF%d)
      TYPE IS (FloatArray3d)
         IF (ASSOCIATED(SELF%d)) DEALLOCATE(SELF%d)
      TYPE IS (FloatArray4d)
         IF (ASSOCIATED(SELF%d)) DEALLOCATE(SELF%d)
      END SELECT
   END SUBROUTINE free_memory

   SUBROUTINE allocate_internal(SELF,msize)
      CLASS(FloatArray) :: SELF
      INTEGER, INTENT(in), OPTIONAL :: msize(:) ! array shape to allocate; optional because not needed for 0d
      ! Allocate memory locally for the %d member of the FloatArray subclasses

      IF (SELF%external_data) THEN
         WRITE(*,*) "%allocate_internal: warning, class member %d associated with external storage. Ignoring the call"
         RETURN  
      END IF

      ! Maybe theres a better way??
      SELECT TYPE(SELF)
      TYPE IS (FloatArray0d)
         IF (ASSOCIATED(SELF%d)) WRITE(*,*) "PRINT EXCEPTION; D ALREADY ASSOCIATED"
         ALLOCATE(SELF%d); SELF%d = 0.
      TYPE IS (FloatArray1d)
         IF (ASSOCIATED(SELF%d)) WRITE(*,*) "PRINT EXCEPTION; D ALREADY ASSOCIATED"
         IF (.NOT. PRESENT(msize)) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC NOT PRESENT"
         IF (SIZE(msize) /= 1) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC DOES NOT CONFORM TO EXPECTED SHAPE"
         ALLOCATE(SELF%d(msize(1)))
      TYPE IS (FloatArray2d)
         IF (ASSOCIATED(SELF%d)) WRITE(*,*) "PRINT EXCEPTION; D ALREADY ASSOCIATED"
         IF (.NOT. PRESENT(msize)) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC NOT PRESENT"
         IF (SIZE(msize) /= 2) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC DOES NOT CONFORM TO EXPECTED SHAPE"  
         ALLOCATE(SELF%d(msize(1),msize(2)))     
      TYPE IS (FloatArray3d)
         IF (ASSOCIATED(SELF%d)) WRITE(*,*) "PRINT EXCEPTION; D ALREADY ASSOCIATED"
         IF (.NOT. PRESENT(msize)) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC NOT PRESENT"
         IF (SIZE(msize) /= 3) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC DOES NOT CONFORM TO EXPECTED SHAPE"     
         ALLOCATE(SELF%d(msize(1),msize(2),msize(3)))    
      TYPE IS (FloatArray4d)
         IF (ASSOCIATED(SELF%d)) WRITE(*,*) "PRINT EXCEPTION; D ALREADY ASSOCIATED"
         IF (.NOT. PRESENT(msize)) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC NOT PRESENT"
         IF (SIZE(msize) /= 4) WRITE(*,*) "PRINT EXCEPTION; SIZE SPEC DOES NOT CONFORM TO EXPECTED SHAPE" 
         ALLOCATE(SELF%d(msize(1),msize(2),msize(3),msize(4)))       
      END SELECT

   END SUBROUTINE allocate_internal

END MODULE mo_structured_datatypes
