      SUBROUTINE CS1S2 (ZR, S1, S2, NZ, ASCLE, ALIM, IUF)
      COMPLEX CZERO, C1, S1, S1D, S2, ZR
      REAL AA, ALIM, ALN, ASCLE, AS1, AS2, XX
      INTEGER IUF, NZ
      DATA CZERO / (0.0E0,0.0E0) /
C***FIRST EXECUTABLE STATEMENT  CS1S2
      NZ = 0
      AS1 = ABS(S1)
      AS2 = ABS(S2)
      AA = REAL(S1)
      ALN = AIMAG(S1)
      IF (AA.EQ.0.0E0 .AND. ALN.EQ.0.0E0) GO TO 10
      IF (AS1.EQ.0.0E0) GO TO 10
      XX = REAL(ZR)
      ALN = -XX - XX + ALOG(AS1)
      S1D = S1
      S1 = CZERO
      AS1 = 0.0E0
      IF (ALN.LT.(-ALIM)) GO TO 10
      C1 = CLOG(S1D) - ZR - ZR
      S1 = CEXP(C1)
      AS1 = ABS(S1)
      IUF = IUF + 1
   10 CONTINUE
      AA = MAX(AS1,AS2)
      IF (AA.GT.ASCLE) RETURN
      S1 = CZERO
      S2 = CZERO
      NZ = 1
      IUF = 0
      RETURN
      END
