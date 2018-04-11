      SUBROUTINE CUOIK (Z, FNU, KODE, IKFLG, N, Y, NUF, TOL, ELIM, ALIM)
      COMPLEX ARG, ASUM, BSUM, CWRK, CZ, CZERO, PHI, SUM, Y, Z, ZB,
     * ZETA1, ZETA2, ZN, ZR
      REAL AARG, AIC, ALIM, APHI, ASCLE, AX, AY, ELIM, FNN, FNU, GNN,
     * GNU, RCZ, TOL, X, YY, R1MACH
      INTEGER I, IFORM, IKFLG, INIT, KODE, N, NN, NUF, NW
      DIMENSION Y(N), CWRK(16)
      DATA CZERO / (0.0E0,0.0E0) /
      DATA AIC / 1.265512123484645396E+00 /
C***FIRST EXECUTABLE STATEMENT  CUOIK
      NUF = 0
      NN = N
      X = REAL(Z)
      ZR = Z
      IF (X.LT.0.0E0) ZR = -Z
      ZB = ZR
      YY = AIMAG(ZR)
      AX = ABS(X)*1.7321E0
      AY = ABS(YY)
      IFORM = 1
      IF (AY.GT.AX) IFORM = 2
      GNU = MAX(FNU,1.0E0)
      IF (IKFLG.EQ.1) GO TO 10
      FNN = NN
      GNN = FNU + FNN - 1.0E0
      GNU = MAX(GNN,FNN)
   10 CONTINUE
C-----------------------------------------------------------------------
C     ONLY THE MAGNITUDE OF ARG AND PHI ARE NEEDED ALONG WITH THE
C     REAL PARTS OF ZETA1, ZETA2 AND ZB. NO ATTEMPT IS MADE TO GET
C     THE SIGN OF THE IMAGINARY PART CORRECT.
C-----------------------------------------------------------------------
      IF (IFORM.EQ.2) GO TO 20
      INIT = 0
      CALL CUNIK(ZR, GNU, IKFLG, 1, TOL, INIT, PHI, ZETA1, ZETA2, SUM,
     * CWRK)
      CZ = -ZETA1 + ZETA2
      GO TO 40
   20 CONTINUE
      ZN = -ZR*CMPLX(0.0E0,1.0E0)
      IF (YY.GT.0.0E0) GO TO 30
      ZN = CONJG(-ZN)
   30 CONTINUE
      CALL CUNHJ(ZN, GNU, 1, TOL, PHI, ARG, ZETA1, ZETA2, ASUM, BSUM)
      CZ = -ZETA1 + ZETA2
      AARG = ABS(ARG)
   40 CONTINUE
      IF (KODE.EQ.2) CZ = CZ - ZB
      IF (IKFLG.EQ.2) CZ = -CZ
      APHI = ABS(PHI)
      RCZ = REAL(CZ)
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      IF (RCZ.GT.ELIM) GO TO 170
      IF (RCZ.LT.ALIM) GO TO 50
      RCZ = RCZ + ALOG(APHI)
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25E0*ALOG(AARG) - AIC
      IF (RCZ.GT.ELIM) GO TO 170
      GO TO 100
   50 CONTINUE
C-----------------------------------------------------------------------
C     UNDERFLOW TEST
C-----------------------------------------------------------------------
      IF (RCZ.LT.(-ELIM)) GO TO 60
      IF (RCZ.GT.(-ALIM)) GO TO 100
      RCZ = RCZ + ALOG(APHI)
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25E0*ALOG(AARG) - AIC
      IF (RCZ.GT.(-ELIM)) GO TO 80
   60 CONTINUE
      DO 70 I=1,NN
        Y(I) = CZERO
   70 CONTINUE
      NUF = NN
      RETURN
   80 CONTINUE
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      CZ = CZ + CLOG(PHI)
      IF (IFORM.EQ.1) GO TO 90
      CZ = CZ - CMPLX(0.25E0,0.0E0)*CLOG(ARG) - CMPLX(AIC,0.0E0)
   90 CONTINUE
      AX = EXP(RCZ)/TOL
      AY = AIMAG(CZ)
      CZ = CMPLX(AX,0.0E0)*CMPLX(COS(AY),SIN(AY))
      CALL CUCHK(CZ, NW, ASCLE, TOL)
      IF (NW.EQ.1) GO TO 60
  100 CONTINUE
      IF (IKFLG.EQ.2) RETURN
      IF (N.EQ.1) RETURN
C-----------------------------------------------------------------------
C     SET UNDERFLOWS ON I SEQUENCE
C-----------------------------------------------------------------------
  110 CONTINUE
      GNU = FNU + (NN-1)
      IF (IFORM.EQ.2) GO TO 120
      INIT = 0
      CALL CUNIK(ZR, GNU, IKFLG, 1, TOL, INIT, PHI, ZETA1, ZETA2, SUM,
     * CWRK)
      CZ = -ZETA1 + ZETA2
      GO TO 130
  120 CONTINUE
      CALL CUNHJ(ZN, GNU, 1, TOL, PHI, ARG, ZETA1, ZETA2, ASUM, BSUM)
      CZ = -ZETA1 + ZETA2
      AARG = ABS(ARG)
  130 CONTINUE
      IF (KODE.EQ.2) CZ = CZ - ZB
      APHI = ABS(PHI)
      RCZ = REAL(CZ)
      IF (RCZ.LT.(-ELIM)) GO TO 140
      IF (RCZ.GT.(-ALIM)) RETURN
      RCZ = RCZ + ALOG(APHI)
      IF (IFORM.EQ.2) RCZ = RCZ - 0.25E0*ALOG(AARG) - AIC
      IF (RCZ.GT.(-ELIM)) GO TO 150
  140 CONTINUE
      Y(NN) = CZERO
      NN = NN - 1
      NUF = NUF + 1
      IF (NN.EQ.0) RETURN
      GO TO 110
  150 CONTINUE
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      CZ = CZ + CLOG(PHI)
      IF (IFORM.EQ.1) GO TO 160
      CZ = CZ - CMPLX(0.25E0,0.0E0)*CLOG(ARG) - CMPLX(AIC,0.0E0)
  160 CONTINUE
      AX = EXP(RCZ)/TOL
      AY = AIMAG(CZ)
      CZ = CMPLX(AX,0.0E0)*CMPLX(COS(AY),SIN(AY))
      CALL CUCHK(CZ, NW, ASCLE, TOL)
      IF (NW.EQ.1) GO TO 140
      RETURN
  170 CONTINUE
      NUF = -1
      RETURN
      END
