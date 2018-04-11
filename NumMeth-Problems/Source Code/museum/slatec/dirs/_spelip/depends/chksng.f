      SUBROUTINE CHKSNG (MBDCND, NBDCND, ALPHA, BETA, GAMA, XNU, COFX,
     +   COFY, SINGLR)
C
      COMMON /SPLPCM/ KSWX       ,KSWY       ,K          ,L          ,
     1                AIT        ,BIT        ,CIT        ,DIT        ,
     2                MIT        ,NIT        ,IS         ,MS         ,
     3                JS         ,NS         ,DLX        ,DLY        ,
     4                TDLX3      ,TDLY3      ,DLX4       ,DLY4
      LOGICAL         SINGLR
C***FIRST EXECUTABLE STATEMENT  CHKSNG
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
   10 IF (NBDCND .NE. 3) GO TO  20
      IF (GAMA.NE.0.0 .OR. XNU.NE.0.0) RETURN
   20 CONTINUE
C
C     CHECK THAT NON-DERIVATIVE COEFFICIENT FUNCTIONS
C     ARE ZERO
C
      DO  30 I=IS,MS
         XI = AIT+(I-1)*DLX
         CALL COFX (XI,AI,BI,CI)
         IF (CI .NE. 0.0) RETURN
   30 CONTINUE
      DO  40 J=JS,NS
         YJ = CIT+(J-1)*DLY
         CALL COFY (YJ,DJ,EJ,FJ)
         IF (FJ .NE. 0.0) RETURN
   40 CONTINUE
C
C     THE OPERATOR MUST BE SINGULAR IF THIS POINT IS REACHED
C
      SINGLR = .TRUE.
      RETURN
      END
