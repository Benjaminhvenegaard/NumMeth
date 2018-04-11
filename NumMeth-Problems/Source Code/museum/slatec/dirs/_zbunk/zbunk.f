      SUBROUTINE ZBUNK (ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, TOL, ELIM,
     +   ALIM)
C     COMPLEX Y,Z
      DOUBLE PRECISION ALIM, AX, AY, ELIM, FNU, TOL, YI, YR, ZI, ZR
      INTEGER KODE, MR, N, NZ
      DIMENSION YR(N), YI(N)
C***FIRST EXECUTABLE STATEMENT  ZBUNK
      NZ = 0
      AX = ABS(ZR)*1.7321D0
      AY = ABS(ZI)
      IF (AY.GT.AX) GO TO 10
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR K(FNU,Z) FOR LARGE FNU APPLIED IN
C     -PI/3.LE.ARG(Z).LE.PI/3
C-----------------------------------------------------------------------
      CALL ZUNK1(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, TOL, ELIM, ALIM)
      GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR H(2,FNU,Z*EXP(M*HPI)) FOR LARGE FNU
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I
C     AND HPI=PI/2
C-----------------------------------------------------------------------
      CALL ZUNK2(ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, TOL, ELIM, ALIM)
   20 CONTINUE
      RETURN
      END
