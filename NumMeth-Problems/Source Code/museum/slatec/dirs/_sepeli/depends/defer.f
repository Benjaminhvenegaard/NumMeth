      SUBROUTINE DEFER (COFX, COFY, IDMN, USOL, GRHS)
C
      COMMON /SPLPCM/ KSWX       ,KSWY       ,K          ,L          ,
     1                AIT        ,BIT        ,CIT        ,DIT        ,
     2                MIT        ,NIT        ,IS         ,MS         ,
     3                JS         ,NS         ,DLX        ,DLY        ,
     4                TDLX3      ,TDLY3      ,DLX4       ,DLY4
      DIMENSION       GRHS(IDMN,*)           ,USOL(IDMN,*)
      EXTERNAL        COFX       ,COFY
C***FIRST EXECUTABLE STATEMENT  DEFER
      DO  40 J=JS,NS
         YJ = CIT+(J-1)*DLY
         CALL COFY (YJ,DJ,EJ,FJ)
         DO  30 I=IS,MS
            XI = AIT+(I-1)*DLX
            CALL COFX (XI,AI,BI,CI)
C
C     COMPUTE PARTIAL DERIVATIVE APPROXIMATIONS AT (XI,YJ)
C
            CALL DX (USOL,IDMN,I,J,UXXX,UXXXX)
            CALL DY (USOL,IDMN,I,J,UYYY,UYYYY)
            TX = AI*UXXXX/12.0+BI*UXXX/6.0
            TY = DJ*UYYYY/12.0+EJ*UYYY/6.0
C
C     RESET FORM OF TRUNCATION IF AT BOUNDARY WHICH IS NON-PERIODIC
C
            IF (KSWX.EQ.1 .OR. (I.GT.1 .AND. I.LT.K)) GO TO  10
            TX = AI/3.0*(UXXXX/4.0+UXXX/DLX)
   10       IF (KSWY.EQ.1 .OR. (J.GT.1 .AND. J.LT.L)) GO TO  20
            TY = DJ/3.0*(UYYYY/4.0+UYYY/DLY)
   20       GRHS(I,J) = GRHS(I,J)+DLX**2*TX+DLY**2*TY
   30    CONTINUE
   40 CONTINUE
C
C     RESET THE RIGHT HAND SIDE IN USOL
C
      DO  60 I=IS,MS
         DO  50 J=JS,NS
            USOL(I,J) = GRHS(I,J)
   50    CONTINUE
   60 CONTINUE
      RETURN
      END
