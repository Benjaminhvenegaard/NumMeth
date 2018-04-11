      SUBROUTINE XNRMP (NU, MU1, MU2, SARG, MODE, SPN, IPN, ISIG,
     1   IERROR)
      INTEGER NU, MU1, MU2, MODE, IPN, ISIG
      REAL SARG, SPN
      DIMENSION SPN(*), IPN(*)
      REAL C1,C2,P,P1,P2,P3,S,SX,T,TX,X,RK
C CALL XSET TO INITIALIZE EXTENDED-RANGE ARITHMETIC (SEE XSET
C LISTING FOR DETAILS)
C***FIRST EXECUTABLE STATEMENT  XNRMP
      IERROR=0
      CALL XSET (0, 0, 0.0, 0,IERROR)
      IF (IERROR.NE.0) RETURN
C
C        TEST FOR PROPER INPUT VALUES.
C
      IF (NU.LT.0) GO TO 110
      IF (MU1.LT.0) GO TO 110
      IF (MU1.GT.MU2) GO TO 110
      IF (NU.EQ.0) GO TO 90
      IF (MODE.LT.1 .OR. MODE.GT.2) GO TO 110
      GO TO (10, 20), MODE
   10 IF (ABS(SARG).GT.1.0) GO TO 120
      IF (ABS(SARG).EQ.1.0) GO TO 90
      X = SARG
      SX = SQRT((1.0+ABS(X))*((0.5-ABS(X))+0.5))
      TX = X/SX
      ISIG = LOG10(2.0*NU*(5.0+TX**2))
      GO TO 30
   20 IF (ABS(SARG).GT.4.0*ATAN(1.0)) GO TO 120
      IF (SARG.EQ.0.0) GO TO 90
      X = COS(SARG)
      SX = ABS(SIN(SARG))
      TX = X/SX
      ISIG = LOG10(2.0*NU*(5.0+ABS(SARG*TX)))
C
C        BEGIN CALCULATION
C
   30 MU = MU2
      I = MU2 - MU1 + 1
C
C        IF MU.GT.NU, NORMALIZED LEGENDRE(NU,MU,X)=0.
C
   40 IF (MU.LE.NU) GO TO 50
      SPN(I) = 0.0
      IPN(I) = 0
      I = I - 1
      MU = MU - 1
      IF (I .GT. 0) GO TO 40
      ISIG = 0
      GO TO 160
   50 MU = NU
C
C        P1 = 0. = NORMALIZED LEGENDRE(NU,NU+1,X)
C
      P1 = 0.0
      IP1 = 0
C
C        CALCULATE P2 = NORMALIZED LEGENDRE(NU,NU,X)
C
      P2 = 1.0
      IP2 = 0
      P3 = 0.5
      RK = 2.0
      DO 60 J=1,NU
        P3 = ((RK+1.0)/RK)*P3
        P2 = P2*SX
        CALL XADJ(P2, IP2,IERROR)
        IF (IERROR.NE.0) RETURN
        RK = RK + 2.0
   60 CONTINUE
      P2 = P2*SQRT(P3)
      CALL XADJ(P2, IP2,IERROR)
      IF (IERROR.NE.0) RETURN
      S = 2.0*TX
      T = 1.0/NU
      IF (MU2.LT.NU) GO TO 70
      SPN(I) = P2
      IPN(I) = IP2
      I = I - 1
      IF (I .EQ. 0) GO TO 140
C
C        RECURRENCE PROCESS
C
   70 P = MU*T
      C1 = 1.0/SQRT((1.0-P+T)*(1.0+P))
      C2 = S*P*C1*P2
      C1 = -SQRT((1.0+P+T)*(1.0-P))*C1*P1
      CALL XADD(C2, IP2, C1, IP1, P, IP,IERROR)
      IF (IERROR.NE.0) RETURN
      MU = MU - 1
      IF (MU.GT.MU2) GO TO 80
C
C        STORE IN ARRAY SPN FOR RETURN TO CALLING ROUTINE.
C
      SPN(I) = P
      IPN(I) = IP
      I = I - 1
      IF (I .EQ. 0) GO TO 140
   80 P1 = P2
      IP1 = IP2
      P2 = P
      IP2 = IP
      IF (MU.LE.MU1) GO TO 140
      GO TO 70
C
C        SPECIAL CASE WHEN X=-1 OR +1, OR NU=0.
C
   90 K = MU2 - MU1 + 1
      DO 100 I=1,K
        SPN(I) = 0.0
        IPN(I) = 0
  100 CONTINUE
      ISIG = 0
      IF (MU1.GT.0) GO TO 160
      ISIG = 1
      SPN(1) = SQRT(NU+0.5)
      IPN(1) = 0
      IF (MOD(NU,2).EQ.0) GO TO 160
      IF (MODE.EQ.1 .AND. SARG.EQ.1.0) GO TO 160
      IF (MODE.EQ.2) GO TO 160
      SPN(1) = -SPN(1)
      GO TO 160
C
C          ERROR PRINTOUTS AND TERMINATION.
C
  110 CALL XERMSG ('SLATEC', 'XNRMP', 'NU, MU1, MU2 or MODE not valid',
     +             112, 1)
      IERROR=112
      RETURN
  120 CALL XERMSG ('SLATEC', 'XNRMP', 'SARG out of range', 113, 1)
      IERROR=113
      RETURN
C
C        RETURN TO CALLING PROGRAM
C
  140 K = MU2 - MU1 + 1
      DO 150 I=1,K
        CALL XRED(SPN(I),IPN(I),IERROR)
        IF (IERROR.NE.0) RETURN
  150 CONTINUE
  160 RETURN
      END
