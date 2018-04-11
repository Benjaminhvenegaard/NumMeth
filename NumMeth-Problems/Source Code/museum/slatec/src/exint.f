      SUBROUTINE EXINT (X, N, KODE, M, TOL, EN, NZ, IERR)
      REAL             A,AA,AAMS,AH,AK,AT,B,BK,BT,CC,CNORM,CT,EM,EMX,EN,
     1                 ETOL,FNM,FX,PT,P1,P2,S,TOL,TX,X,XCUT,XLIM,XTOL,Y,
     2                 YT,Y1,Y2
      REAL             R1MACH,PSIXN
      INTEGER I,IC,ICASE,ICT,IERR,IK,IND,IX,I1M,JSET,K,KK,KN,KODE,KS,M,
     1        ML,MU,N,ND,NM,NZ
      INTEGER I1MACH
      DIMENSION EN(*), A(99), B(99), Y(2)
C***FIRST EXECUTABLE STATEMENT  EXINT
      IERR = 0
      NZ = 0
      ETOL = MAX(R1MACH(4),0.5E-18)
      IF (X.LT.0.0E0) IERR = 1
      IF (N.LT.1) IERR = 1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR = 1
      IF (M.LT.1) IERR = 1
      IF (TOL.LT.ETOL .OR. TOL.GT.0.1E0) IERR = 1
      IF (X.EQ.0.0E0 .AND. N.EQ.1) IERR = 1
      IF (IERR.NE.0) RETURN
      I1M = -I1MACH(12)
      PT = 2.3026E0*R1MACH(5)*I1M
      XLIM = PT - 6.907755E0
      BT = PT + (N+M-1)
      IF (BT.GT.1000.0E0) XLIM = PT - LOG(BT)
C
      XCUT = 2.0E0
      IF (ETOL.GT.2.0E-7) XCUT = 1.0E0
      IF (X.GT.XCUT) GO TO 100
      IF (X.EQ.0.0E0 .AND. N.GT.1) GO TO 80
C-----------------------------------------------------------------------
C     SERIES FOR E(N,X) FOR X.LE.XCUT
C-----------------------------------------------------------------------
      TX = X + 0.5E0
      IX = TX
C-----------------------------------------------------------------------
C     ICASE=1 MEANS INTEGER CLOSEST TO X IS 2 AND N=1
C     ICASE=2 MEANS INTEGER CLOSEST TO X IS 0,1, OR 2 AND N.GE.2
C-----------------------------------------------------------------------
      ICASE = 2
      IF (IX.GT.N) ICASE = 1
      NM = N - ICASE + 1
      ND = NM + 1
      IND = 3 - ICASE
      MU = M - IND
      ML = 1
      KS = ND
      FNM = NM
      S = 0.0E0
      XTOL = 3.0E0*TOL
      IF (ND.EQ.1) GO TO 10
      XTOL = 0.3333E0*TOL
      S = 1.0E0/FNM
   10 CONTINUE
      AA = 1.0E0
      AK = 1.0E0
      IC = 35
      IF (X.LT.ETOL) IC = 1
      DO 50 I=1,IC
        AA = -AA*X/AK
        IF (I.EQ.NM) GO TO 30
        S = S - AA/(AK-FNM)
        IF (ABS(AA).LE.XTOL*ABS(S)) GO TO 20
        AK = AK + 1.0E0
        GO TO 50
   20   CONTINUE
        IF (I.LT.2) GO TO 40
        IF (ND-2.GT.I .OR. I.GT.ND-1) GO TO 60
        AK = AK + 1.0E0
        GO TO 50
   30   S = S + AA*(-LOG(X)+PSIXN(ND))
        XTOL = 3.0E0*TOL
   40   AK = AK + 1.0E0
   50 CONTINUE
      IF (IC.NE.1) GO TO 340
   60 IF (ND.EQ.1) S = S + (-LOG(X)+PSIXN(1))
      IF (KODE.EQ.2) S = S*EXP(X)
      EN(1) = S
      EMX = 1.0E0
      IF (M.EQ.1) GO TO 70
      EN(IND) = S
      AA = KS
      IF (KODE.EQ.1) EMX = EXP(-X)
      GO TO (220, 240), ICASE
   70 IF (ICASE.EQ.2) RETURN
      IF (KODE.EQ.1) EMX = EXP(-X)
      EN(1) = (EMX-S)/X
      RETURN
   80 CONTINUE
      DO 90 I=1,M
        EN(I) = 1.0E0/(N+I-2)
   90 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     BACKWARD RECURSIVE MILLER ALGORITHM FOR
C              E(N,X)=EXP(-X)*(X**(N-1))*U(N,N,X)
C     WITH RECURSION AWAY FROM N=INTEGER CLOSEST TO X.
C     U(A,B,X) IS THE SECOND CONFLUENT HYPERGEOMETRIC FUNCTION
C-----------------------------------------------------------------------
  100 CONTINUE
      EMX = 1.0E0
      IF (KODE.EQ.2) GO TO 130
      IF (X.LE.XLIM) GO TO 120
      NZ = M
      DO 110 I=1,M
        EN(I) = 0.0E0
  110 CONTINUE
      RETURN
  120 EMX = EXP(-X)
  130 CONTINUE
      IX = X+0.5E0
      KN = N + M - 1
      IF (KN.LE.IX) GO TO 140
      IF (N.LT.IX .AND. IX.LT.KN) GO TO 170
      IF (N.GE.IX) GO TO 160
      GO TO 340
  140 ICASE = 1
      KS = KN
      ML = M - 1
      MU = -1
      IND = M
      IF (KN.GT.1) GO TO 180
  150 KS = 2
      ICASE = 3
      GO TO 180
  160 ICASE = 2
      IND = 1
      KS = N
      MU = M - 1
      IF (N.GT.1) GO TO 180
      IF (KN.EQ.1) GO TO 150
      IX = 2
  170 ICASE = 1
      KS = IX
      ML = IX - N
      IND = ML + 1
      MU = KN - IX
  180 CONTINUE
      IK = KS/2
      AH = IK
      JSET = 1 + KS - (IK+IK)
C-----------------------------------------------------------------------
C     START COMPUTATION FOR
C              EN(IND) = C*U( A , A ,X)    JSET=1
C              EN(IND) = C*U(A+1,A+1,X)    JSET=2
C     FOR AN EVEN INTEGER A.
C-----------------------------------------------------------------------
      IC = 0
      AA = AH + AH
      AAMS = AA - 1.0E0
      AAMS = AAMS*AAMS
      TX = X + X
      FX = TX + TX
      AK = AH
      XTOL = TOL
      IF (TOL.LE.1.0E-3) XTOL = 20.0E0*TOL
      CT = AAMS + FX*AH
      EM = (AH+1.0E0)/((X+AA)*XTOL*SQRT(CT))
      BK = AA
      CC = AH*AH
C-----------------------------------------------------------------------
C     FORWARD RECURSION FOR P(IC),P(IC+1) AND INDEX IC FOR BACKWARD
C     RECURSION
C-----------------------------------------------------------------------
      P1 = 0.0E0
      P2 = 1.0E0
  190 CONTINUE
      IF (IC.EQ.99) GO TO 340
      IC = IC + 1
      AK = AK + 1.0E0
      AT = BK/(BK+AK+CC+IC)
      BK = BK + AK + AK
      A(IC) = AT
      BT = (AK+AK+X)/(AK+1.0E0)
      B(IC) = BT
      PT = P2
      P2 = BT*P2 - AT*P1
      P1 = PT
      CT = CT + FX
      EM = EM*AT*(1.0E0-TX/CT)
      IF (EM*(AK+1.0E0).GT.P1*P1) GO TO 190
      ICT = IC
      KK = IC + 1
      BT = TX/(CT+FX)
      Y2 = (BK/(BK+CC+KK))*(P1/P2)*(1.0E0-BT+0.375E0*BT*BT)
      Y1 = 1.0E0
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE FOR
C              Y1=             C*U( A ,A,X)
C              Y2= C*(A/(1+A/2))*U(A+1,A,X)
C-----------------------------------------------------------------------
      DO 200 K=1,ICT
        KK = KK - 1
        YT = Y1
        Y1 = (B(KK)*Y1-Y2)/A(KK)
        Y2 = YT
  200 CONTINUE
C-----------------------------------------------------------------------
C     THE CONTIGUOUS RELATION
C              X*U(B,C+1,X)=(C-B)*U(B,C,X)+U(B-1,C,X)
C     WITH  B=A+1 , C=A IS USED FOR
C              Y(2) = C * U(A+1,A+1,X)
C     X IS INCORPORATED INTO THE NORMALIZING RELATION
C-----------------------------------------------------------------------
      PT = Y2/Y1
      CNORM = 1.0E0 - PT*(AH+1.0E0)/AA
      Y(1) = 1.0E0/(CNORM*AA+X)
      Y(2) = CNORM*Y(1)
      IF (ICASE.EQ.3) GO TO 210
      EN(IND) = EMX*Y(JSET)
      IF (M.EQ.1) RETURN
      AA = KS
      GO TO (220, 240), ICASE
C-----------------------------------------------------------------------
C     RECURSION SECTION  N*E(N+1,X) + X*E(N,X)=EMX
C-----------------------------------------------------------------------
  210 EN(1) = EMX*(1.0E0-Y(1))/X
      RETURN
  220 K = IND - 1
      DO 230 I=1,ML
        AA = AA - 1.0E0
        EN(K) = (EMX-AA*EN(K+1))/X
        K = K - 1
  230 CONTINUE
      IF (MU.LE.0) RETURN
      AA = KS
  240 K = IND
      DO 250 I=1,MU
        EN(K+1) = (EMX-X*EN(K))/AA
        AA = AA + 1.0E0
        K = K + 1
  250 CONTINUE
      RETURN
  340 CONTINUE
      IERR = 2
      RETURN
      END
