      SUBROUTINE CHKSN4 (MBDCND, NBDCND, ALPHA, BETA, COFX, SINGLR)
C
      COMMON /SPL4/   KSWX       ,KSWY       ,K          ,L          ,
     1                AIT        ,BIT        ,CIT        ,DIT        ,
     2                MIT        ,NIT        ,IS         ,MS         ,
     3                JS         ,NS         ,DLX        ,DLY        ,
     4                TDLX3      ,TDLY3      ,DLX4       ,DLY4
      LOGICAL         SINGLR
      EXTERNAL COFX
C***FIRST EXECUTABLE STATEMENT  CHKSN4
      SINGLR = .FALSE.
C
C     CHECK IF THE BOUNDARY CONDITIONS ARE
C     ENTIRELY PERIODIC AND/OR MIXED
C
      IF ((MBDCND.NE.0 .AND. MBDCND.NE.3) .OR.
     1    (NBDCND.NE.0 .AND. NBDCND.NE.3)) RETURN
C
C     CHECK THAT MIXED CONDITIONS ARE PURE NEUMAN
C
      IF (MBDCND .NE. 3) GO TO  10
      IF (ALPHA.NE.0.0 .OR. BETA.NE.0.0) RETURN
   10 CONTINUE
C
C     CHECK THAT NON-DERIVATIVE COEFFICIENT FUNCTIONS
C     ARE ZERO
C
      DO  30 I=IS,MS
         XI = AIT+(I-1)*DLX
         CALL COFX (XI,AI,BI,CI)
         IF (CI .NE. 0.0) RETURN
   30 CONTINUE
C
C     THE OPERATOR MUST BE SINGULAR IF THIS POINT IS REACHED
C
      SINGLR = .TRUE.
      RETURN
      END
