      SUBROUTINE RC3JJ (L2, L3, M2, M3, L1MIN, L1MAX, THRCOF, NDIM, IER)
C
      INTEGER NDIM, IER
      REAL L2, L3, M2, M3, L1MIN, L1MAX, THRCOF(NDIM)
C
      INTEGER I, INDEX, LSTEP, N, NFIN, NFINP1, NFINP2, NFINP3, NLIM,
     +        NSTEP2
      REAL A1, A1S, A2, A2S, C1, C1OLD, C2, CNORM, R1MACH,
     +     DENOM, DV, EPS, HUGE, L1, M1, NEWFAC, OLDFAC,
     +     ONE, RATIO, SIGN1, SIGN2, SRHUGE, SRTINY, SUM1,
     +     SUM2, SUMBAC, SUMFOR, SUMUNI, THREE, THRESH,
     +     TINY, TWO, X, X1, X2, X3, Y, Y1, Y2, Y3, ZERO
C
      DATA  ZERO,EPS,ONE,TWO,THREE /0.0,0.01,1.0,2.0,3.0/
C
C***FIRST EXECUTABLE STATEMENT  RC3JJ
      IER=0
C  HUGE is the square root of one twentieth of the largest floating
C  point number, approximately.
      HUGE = SQRT(R1MACH(2)/20.0)
      SRHUGE = SQRT(HUGE)
      TINY = 1.0/HUGE
      SRTINY = 1.0/SRHUGE
C
C     LMATCH = ZERO
      M1 = - M2 - M3
C
C  Check error conditions 1 and 2.
      IF((L2-ABS(M2)+EPS.LT.ZERO).OR.
     +   (L3-ABS(M3)+EPS.LT.ZERO))THEN
         IER=1
         CALL XERMSG('SLATEC','RC3JJ','L2-ABS(M2) or L3-ABS(M3) '//
     +      'less than zero.',IER,1)
         RETURN
      ELSEIF((MOD(L2+ABS(M2)+EPS,ONE).GE.EPS+EPS).OR.
     +   (MOD(L3+ABS(M3)+EPS,ONE).GE.EPS+EPS))THEN
         IER=2
         CALL XERMSG('SLATEC','RC3JJ','L2+ABS(M2) or L3+ABS(M3) '//
     +      'not integer.',IER,1)
         RETURN
      ENDIF
C
C
C
C  Limits for L1
C
      L1MIN = MAX(ABS(L2-L3),ABS(M1))
      L1MAX = L2 + L3
C
C  Check error condition 3.
      IF(MOD(L1MAX-L1MIN+EPS,ONE).GE.EPS+EPS)THEN
         IER=3
         CALL XERMSG('SLATEC','RC3JJ','L1MAX-L1MIN not integer.',IER,1)
         RETURN
      ENDIF
      IF(L1MIN.LT.L1MAX-EPS)   GO TO 20
      IF(L1MIN.LT.L1MAX+EPS)   GO TO 10
C
C  Check error condition 4.
      IER=4
      CALL XERMSG('SLATEC','RC3JJ','L1MIN greater than L1MAX.',IER,1)
      RETURN
C
C  This is reached in case that L1 can take only one value,
C  i.e. L1MIN = L1MAX
C
   10 CONTINUE
C     LSCALE = 0
      THRCOF(1) = (-ONE) ** INT(ABS(L2+M2-L3+M3)+EPS) /
     1 SQRT(L1MIN + L2 + L3 + ONE)
      RETURN
C
C  This is reached in case that L1 takes more than one value,
C  i.e. L1MIN < L1MAX.
C
   20 CONTINUE
C     LSCALE = 0
      NFIN = INT(L1MAX-L1MIN+ONE+EPS)
      IF(NDIM-NFIN)  21, 23, 23
C
C  Check error condition 5.
   21 IER = 5
      CALL XERMSG('SLATEC','RC3JJ','Dimension of result array for 3j '//
     +            'coefficients too small.',IER,1)
      RETURN
C
C
C  Starting forward recursion from L1MIN taking NSTEP1 steps
C
   23 L1 = L1MIN
      NEWFAC = 0.0
      C1 = 0.0
      THRCOF(1) = SRTINY
      SUM1 = (L1+L1+ONE) * TINY
C
C
      LSTEP = 1
   30 LSTEP = LSTEP + 1
      L1 = L1 + ONE
C
C
      OLDFAC = NEWFAC
      A1 = (L1+L2+L3+ONE) * (L1-L2+L3) * (L1+L2-L3) * (-L1+L2+L3+ONE)
      A2 = (L1+M1) * (L1-M1)
      NEWFAC = SQRT(A1*A2)
      IF(L1.LT.ONE+EPS)   GO TO 40
C
C
      DV = - L2*(L2+ONE) * M1 + L3*(L3+ONE) * M1 + L1*(L1-ONE) * (M3-M2)
      DENOM = (L1-ONE) * NEWFAC
C
      IF(LSTEP-2)  32, 32, 31
C
   31 C1OLD = ABS(C1)
   32 C1 = - (L1+L1-ONE) * DV / DENOM
      GO TO 50
C
C  If L1 = 1, (L1-1) has to be factored out of DV, hence
C
   40 C1 = - (L1+L1-ONE) * L1 * (M3-M2) / NEWFAC
C
   50 IF(LSTEP.GT.2)   GO TO 60
C
C
C  If L1 = L1MIN + 1, the third term in the recursion equation vanishes,
C  hence
      X = SRTINY * C1
      THRCOF(2) = X
      SUM1 = SUM1 + TINY * (L1+L1+ONE) * C1*C1
      IF(LSTEP.EQ.NFIN)   GO TO 220
      GO TO 30
C
C
   60 C2 = - L1 * OLDFAC / DENOM
C
C  Recursion to the next 3j coefficient X
C
      X = C1 * THRCOF(LSTEP-1) + C2 * THRCOF(LSTEP-2)
      THRCOF(LSTEP) = X
      SUMFOR = SUM1
      SUM1 = SUM1 + (L1+L1+ONE) * X*X
      IF(LSTEP.EQ.NFIN)   GO TO 100
C
C  See if last unnormalized 3j coefficient exceeds SRHUGE
C
      IF(ABS(X).LT.SRHUGE)   GO TO 80
C
C  This is reached if last 3j coefficient larger than SRHUGE,
C  so that the recursion series THRCOF(1), ... , THRCOF(LSTEP)
C  has to be rescaled to prevent overflow
C
C     LSCALE = LSCALE + 1
      DO 70 I=1,LSTEP
      IF(ABS(THRCOF(I)).LT.SRTINY)   THRCOF(I) = ZERO
   70 THRCOF(I) = THRCOF(I) / SRHUGE
      SUM1 = SUM1 / HUGE
      SUMFOR = SUMFOR / HUGE
      X = X / SRHUGE
C
C  As long as ABS(C1) is decreasing, the recursion proceeds towards
C  increasing 3j values and, hence, is numerically stable.  Once
C  an increase of ABS(C1) is detected, the recursion direction is
C  reversed.
C
   80 IF(C1OLD-ABS(C1))   100, 100, 30
C
C
C  Keep three 3j coefficients around LMATCH for comparison with
C  backward recursion.
C
  100 CONTINUE
C     LMATCH = L1 - 1
      X1 = X
      X2 = THRCOF(LSTEP-1)
      X3 = THRCOF(LSTEP-2)
      NSTEP2 = NFIN - LSTEP + 3
C
C
C
C
C  Starting backward recursion from L1MAX taking NSTEP2 steps, so
C  that forward and backward recursion overlap at three points
C  L1 = LMATCH+1, LMATCH, LMATCH-1.
C
      NFINP1 = NFIN + 1
      NFINP2 = NFIN + 2
      NFINP3 = NFIN + 3
      L1 = L1MAX
      THRCOF(NFIN) = SRTINY
      SUM2 = TINY * (L1+L1+ONE)
C
      L1 = L1 + TWO
      LSTEP = 1
  110 LSTEP = LSTEP + 1
      L1 = L1 - ONE
C
      OLDFAC = NEWFAC
      A1S = (L1+L2+L3)*(L1-L2+L3-ONE)*(L1+L2-L3-ONE)*(-L1+L2+L3+TWO)
      A2S = (L1+M1-ONE) * (L1-M1-ONE)
      NEWFAC = SQRT(A1S*A2S)
C
      DV = - L2*(L2+ONE) * M1 + L3*(L3+ONE) * M1 + L1*(L1-ONE) * (M3-M2)
C
      DENOM = L1 * NEWFAC
      C1 = - (L1+L1-ONE) * DV / DENOM
      IF(LSTEP.GT.2)   GO TO 120
C
C  If L1 = L1MAX + 1, the third term in the recursion formula vanishes
C
      Y = SRTINY * C1
      THRCOF(NFIN-1) = Y
      SUMBAC = SUM2
      SUM2 = SUM2 + TINY * (L1+L1-THREE) * C1*C1
C
      GO TO 110
C
C
  120 C2 = - (L1 - ONE) * OLDFAC / DENOM
C
C  Recursion to the next 3j coefficient Y
C
      Y = C1 * THRCOF(NFINP2-LSTEP) + C2 * THRCOF(NFINP3-LSTEP)
C
      IF(LSTEP.EQ.NSTEP2)   GO TO 200
C
      THRCOF(NFINP1-LSTEP) = Y
      SUMBAC = SUM2
      SUM2 = SUM2 + (L1+L1-THREE) * Y*Y
C
C  See if last unnormalized 3j coefficient exceeds SRHUGE
C
      IF(ABS(Y).LT.SRHUGE)   GO TO 110
C
C  This is reached if last 3j coefficient larger than SRHUGE,
C  so that the recursion series THRCOF(NFIN), ... ,THRCOF(NFIN-LSTEP+1)
C  has to be rescaled to prevent overflow
C
C     LSCALE = LSCALE + 1
      DO 130 I=1,LSTEP
      INDEX = NFIN - I + 1
      IF(ABS(THRCOF(INDEX)).LT.SRTINY)   THRCOF(INDEX) = ZERO
  130 THRCOF(INDEX) = THRCOF(INDEX) / SRHUGE
      SUM2 = SUM2 / HUGE
      SUMBAC = SUMBAC / HUGE
C
C
      GO TO 110
C
C
C  The forward recursion 3j coefficients X1, X2, X3 are to be matched
C  with the corresponding backward recursion values Y1, Y2, Y3.
C
  200 Y3 = Y
      Y2 = THRCOF(NFINP2-LSTEP)
      Y1 = THRCOF(NFINP3-LSTEP)
C
C
C  Determine now RATIO such that YI = RATIO * XI  (I=1,2,3) holds
C  with minimal error.
C
      RATIO = ( X1*Y1 + X2*Y2 + X3*Y3 ) / ( X1*X1 + X2*X2 + X3*X3 )
      NLIM = NFIN - NSTEP2 + 1
C
      IF(ABS(RATIO).LT.ONE)   GO TO 211
C
      DO 210 N=1,NLIM
  210 THRCOF(N) = RATIO * THRCOF(N)
      SUMUNI = RATIO * RATIO * SUMFOR + SUMBAC
      GO TO 230
C
  211 NLIM = NLIM + 1
      RATIO = ONE / RATIO
      DO 212 N=NLIM,NFIN
  212 THRCOF(N) = RATIO * THRCOF(N)
      SUMUNI = SUMFOR + RATIO*RATIO*SUMBAC
      GO TO 230
C
  220 SUMUNI = SUM1
C
C
C  Normalize 3j coefficients
C
  230 CNORM = ONE / SQRT(SUMUNI)
C
C  Sign convention for last 3j coefficient determines overall phase
C
      SIGN1 = SIGN(ONE,THRCOF(NFIN))
      SIGN2 = (-ONE) ** INT(ABS(L2+M2-L3+M3)+EPS)
      IF(SIGN1*SIGN2) 235,235,236
  235 CNORM = - CNORM
C
  236 IF(ABS(CNORM).LT.ONE)   GO TO 250
C
      DO 240 N=1,NFIN
  240 THRCOF(N) = CNORM * THRCOF(N)
      RETURN
C
  250 THRESH = TINY / ABS(CNORM)
      DO 251 N=1,NFIN
      IF(ABS(THRCOF(N)).LT.THRESH)   THRCOF(N) = ZERO
  251 THRCOF(N) = CNORM * THRCOF(N)
C
      RETURN
      END
