! (C) Copyright 2022- ECMWF.
! (C) Copyright 2022- Meteo-France.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM FIELD_VIEW
    USE FIELD_MODULE
    USE FIELD_FACTORY_MODULE
    USE PARKIND1
    USE FIELD_ABORT_MODULE
    USE FIELD_ACCESS_MODULE, ONLY: GET_HOST_DATA_RDWR, GET_DEVICE_DATA_RDWR, GET_HOST_DATA_RDONLY,GET_DEVICE_DATA_RDONLY

    IMPLICIT NONE

    CLASS(FIELD_3RB), POINTER :: FIELD_A => NULL()
    REAL(KIND=JPRB), ALLOCATABLE :: A(:,:,:)
    REAL(KIND=JPRB), ALLOCATABLE :: B(:,:,:)

    REAL(KIND=JPRB), POINTER :: ZZ3 (:,:,:)
    REAL(KIND=JPRB), POINTER :: ZZ1 (:)
    TYPE (FIELD_2RB_VIEW),ALLOCATABLE :: YL2VA(:)

    INTEGER :: JLEV,KLEV,JFLD,KSPEC,KFLDS,JINIT,ID
    LOGICAL :: LDACC, LA2B, LSUCCESS

    LSUCCESS = .TRUE.
    KLEV = 3
    KSPEC = 10
    KFLDS = 2
    ALLOCATE(A(KLEV,KSPEC,KFLDS))
    ALLOCATE(B(KLEV,KSPEC,KFLDS))
    !$ACC ENTER DATA CREATE(B)
    CALL FIELD_NEW(FIELD_A, DATA=A)
    WRITE(*,*) "A", SHAPE(A)
    DO  JINIT = 1, 4

        LDACC = .FALSE.
        LA2B = .FALSE.
        IF (JINIT > 2) LDACC = .TRUE.
        IF (MOD(JINIT,2) == 0) LA2B = .TRUE.

        WRITE(*,"(A,I1,A,L1, A,L1,A)") "CONFIG: ", JINIT, " [LDACC=", LDACC, ", LA2B=", LA2B,"]"

        ZZ3 => GET_HOST_DATA_RDWR(FIELD_A)
 
        IF (LA2B) THEN
          ZZ3(:,:,:) = JINIT
          B(:,:,:) = -1
        ELSE
          ZZ3(:,:,:) = -1
          B(:,:,:) = JINIT
        ENDIF
        if (LDACC) THEN
            !ZZ3 => GET_DEVICE_DATA_RDONLY(FIELD_A)
           !$acc update device (B)
        endif

!     ----------------------------
!     Version 1 - allocating Y2LVA inside LS will generate a
!                 'partially present' error OpenAcc runtime error
      YL2VA = LS(FIELD_A,LA2B,LDACC, KLEV, KFLDS)

!     ----------------------------
!     Version 2 - pre-allocating Y2LVA will solve the issue 
!      ALLOCATE(YL2VA(KLEV*KFLDS))
!      CALL LS_ROUTINE(YL2VA, FIELD_A,LA2B,LDACC, KLEV, KFLDS)

      WRITE(*,*) "RETRIEVE:"
      ID = 1
      DO JLEV = 1,KLEV
         DO JFLD = 1,KFLDS
            ZZ1=>YL2VA(ID)%P
            WRITE(*,"(I3,A,z16.16)") ID, ": 0x", LOC(ZZ1)
            IF (LDACC) THEN
              !$acc kernels present (B, ZZ1)
              IF (LA2B) THEN
                B(JLEV,:,JFLD) = ZZ1(:)
              ELSE
                ZZ1(:) = B(JLEV,:,JFLD)
              ENDIF
             !$acc end kernels
            ELSE
              IF (LA2B) THEN
                B(JLEV,:,JFLD) = ZZ1(:)
              ELSE
                ZZ1(:) = B(JLEV,:,JFLD)
              ENDIF
            ENDIF
            ID = ID + 1
          ENDDO
      ENDDO
      IF (LDACC) THEN
        ! Synchronize on host before testing values
        ZZ3 => GET_HOST_DATA_RDONLY (FIELD_A)
        !$acc update host (B)
      ENDIF

      IF ( .NOT. ALL(A == JINIT))THEN
        WRITE(*,*) "ERROR: A IS NOT ALWAY EQUAL TO ", JINIT 
        WRITE(*,*) A
        LSUCCESS = .FALSE.
      END IF
      IF ( .NOT. ALL(B == JINIT))THEN
        WRITE(*,*) "ERROR: B IS NOT ALWAYS EQUAL TO ", JINIT
        WRITE(*,*) B 
        LSUCCESS = .FALSE.
      END IF

      DEALLOCATE(YL2VA)
    ENDDO

  !$ACC EXIT DATA DELETE(B)

  CALL FIELD_DELETE(FIELD_A)
  DEALLOCATE(A)
  DEALLOCATE(B)

  IF (.NOT. LSUCCESS) THEN
     CALL FIELD_ABORT ("Error occured")
  ENDIF
contains

! In LS_ROUTINE, LS array is pre-allocated and passed as argument.
! This version is supported by nvhpc 22.11

subroutine LS_ROUTINE(LS,FIELD_A,LA2B,LDACC, KLEV, KFLDS)
    TYPE (FIELD_2RB_VIEW),ALLOCATABLE, INTENT(INOUT) :: LS(:)
    INTEGER :: KLEV,KFLDS
    LOGICAL LA2B, LDACC
    CLASS(FIELD_3RB), POINTER :: FIELD_A

    REAL(KIND=JPRB), POINTER :: ZZ1 (:)
    REAL(KIND=JPRB), POINTER :: ZZ3 (:,:,:)
    INTEGER :: ID, JLEV, JFLD

    IF (LA2B) THEN
      IF (LDACC) THEN
          ZZ3 => GET_DEVICE_DATA_RDONLY(FIELD_A)
        ELSE
          ZZ3 => GET_HOST_DATA_RDONLY(FIELD_A)
      ENDIF
    ELSE
      IF (LDACC) THEN
        ZZ3 => GET_DEVICE_DATA_RDWR(FIELD_A)
      ELSE
        ZZ3 => GET_HOST_DATA_RDWR(FIELD_A)
      ENDIF
    ENDIF

  WRITE(*,*) "ASSIGN:"
  ID = 1
  DO JLEV = 1,KLEV
     DO JFLD = 1,KFLDS
        LS(ID)%P => ZZ3 (JLEV, :, JFLD)
        WRITE(*,"(I3,A,z16.16)") ID, ": 0x", LOC(LS(ID)%P)
        ID = ID + 1
      ENDDO
  ENDDO

  end subroutine

! In LS, LS array is allocated inside the function.
! This is not supported by nvhpc 22.11

function LS(FIELD_A,LA2B,LDACC, KLEV, KFLDS)
    TYPE (FIELD_2RB_VIEW),ALLOCATABLE :: LS(:)
    INTEGER :: KLEV,KFLDS
    LOGICAL LA2B, LDACC
    CLASS(FIELD_3RB), POINTER :: FIELD_A
 
    ALLOCATE(LS(KLEV*KFLDS))
    CALL LS_ROUTINE(LS, FIELD_A, LA2B, LDACC, KLEV, KFLDS)
  end function



END PROGRAM FIELD_VIEW
