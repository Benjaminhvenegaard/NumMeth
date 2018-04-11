      SUBROUTINE DRC3JM (L1, L2, L3, M1, M2MIN, M2MAX, THRCOF, NDIM,
     +   IER)
C
      INTEGER NDIM, IER
      DOUBLE PRECISION L1, L2, L3, M1, M2MIN, M2MAX, THRCOF(NDIM)
C
      INTEGER I, INDEX, LSTEP, N, NFIN, NFINP1, NFINP2, NFINP3, NLIM,
     +        NSTEP2
      DOUBLE PRECISION A1, A1S, C1, C1OLD, C2, CNORM, D1MACH, DV, EPS,
     +                 HUGE, M2, M3, NEWFAC, OLDFAC, ONE, RATIO, SIGN1,
     +                 SIGN2, SRHUGE, SRTINY, SUM1, SUM2, SUMBAC,
     +                 SUMFOR, SUMUNI, THRESH, TINY, TWO, X, X1, X2, X3,
     +                 Y, Y1, Y2, Y3, ZERO
C
      DATA  ZERO,EPS,ONE,TWO /0.0D0,0.01D0,1.0D0,2.0D0/
C
C***FIRST EXECUTABLE STATEMENT  DRC3JM
      IER=0
C  HUGE is the square root of one twentieth of the largest floating
C  point number, approximately.
      HUGE = SQRT(D1MACH(2)/20.0D0)
      SRHUGE = SQRT(HUGE)
      TINY = 1.0D0/HUGE
      SRTINY = 1.0D0/SRHUGE
C
C     MMATCH = ZERO
C
C
C  Check error conditions 1, 2, and 3.
      IF((L1-ABS(M1)+EPS.LT.ZERO).OR.
     +   (MOD(L1+ABS(M1)+EPS,ONE).GE.EPS+EPS))THEN
         IER=1
         CALL XERMSG('SLATEC','DRC3JM','L1-ABS(M1) less than zero or '//
     +      'L1+ABS(M1) not integer.',IER,1)
         RETURN
      ELSEIF((L1+L2-L3.LT.-EPS).OR.(L1-L2+L3.LT.-EPS).OR.
     +   (-L1+L2+L3.LT.-EPS))THEN
         IER=2
         CALL XERMSG('SLATEC','DRC3JM','L1, L2, L3 do not satisfy '//
     +      'triangular condition.',IER,1)
         RETURN
      ELSEIF(MOD(L1+L2+L3+EPS,ONE).GE.EPS+EPS)THEN
         IER=3
         CALL XERMSG('SLATEC','DRC3JM','L1+L2+L3 not integer.',IER,1)
         RETURN
      ENDIF
C
C
C  Limits for M2
      M2MIN = MAX(-L2,-L3-M1)
      M2MAX = MIN(L2,L3-M1)
C
C  Check error condition 4.
      IF(MOD(M2MAX-M2MIN+EPS,ONE).GE.EPS+EPS)THEN
         IER=4
         CALL XERMSG('SLATEC','DRC3JM','M2MAX-M2MIN not integer.',IER,1)
         RETURN
      ENDIF
      IF(M2MIN.LT.M2MAX-EPS)   GO TO 20
      IF(M2MIN.LT.M2MAX+EPS)   GO TO 10
C
C  Check error condition 5.
      IER=5
      CALL XERMSG('SLATEC','DRC3JM','M2MIN greater than M2MAX.',IER,1)
      RETURN
C
C
C  This is reached in case that M2 and M3 can take only one value.
   10 CONTINUE
C     MSCALE = 0
      THRCOF(1) = (-ONE) ** INT(ABS(L2-L3-M1)+EPS) /
     1 SQRT(L1+L2+L3+ONE)
      RETURN
C
C  This is reached in case that M1 and M2 take more than one value.
   20 CONTINUE
C     MSCALE = 0
      NFIN = INT(M2MAX-M2MIN+ONE+EPS)
      IF(NDIM-NFIN)   21, 23, 23
C
C  Check error condition 6.
   21 IER = 6
      CALL XERMSG('SLATEC','DRC3JM','Dimension of result array for '//
     +            '3j coefficients too small.',IER,1)
      RETURN
C
C
C
C  Start of forward recursion from M2 = M2MIN
C
   23 M2 = M2MIN
      THRCOF(1) = SRTINY
      NEWFAC = 0.0D0
      C1 = 0.0D0
      SUM1 = TINY
C
C
      LSTEP = 1
   30 LSTEP = LSTEP + 1
      M2 = M2 + ONE
      M3 = - M1 - M2
C
C
      OLDFAC = NEWFAC
      A1 = (L2-M2+ONE) * (L2+M2) * (L3+M3+ONE) * (L3-M3)
      NEWFAC = SQRT(A1)
C
C
      DV = (L1+L2+L3+ONE)*(L2+L3-L1) - (L2-M2+ONE)*(L3+M3+ONE)
     1                               - (L2+M2-ONE)*(L3-M3-ONE)
C
      IF(LSTEP-2)  32, 32, 31
C
   31 C1OLD = ABS(C1)
   32 C1 = - DV / NEWFAC
C
      IF(LSTEP.GT.2)   GO TO 60
C
C
C  If M2 = M2MIN + 1, the third term in the recursion equation vanishes,
C  hence
C
      X = SRTINY * C1
      THRCOF(2) = X
      SUM1 = SUM1 + TINY * C1*C1
      IF(LSTEP.EQ.NFIN)   GO TO 220
      GO TO 30
C
C
   60 C2 = - OLDFAC / NEWFAC
C
C  Recursion to the next 3j coefficient
      X = C1 * THRCOF(LSTEP-1) + C2 * THRCOF(LSTEP-2)
      THRCOF(LSTEP) = X
      SUMFOR = SUM1
      SUM1 = SUM1 + X*X
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
C     MSCALE = MSCALE + 1
      DO 70 I=1,LSTEP
      IF(ABS(THRCOF(I)).LT.SRTINY)   THRCOF(I) = ZERO
   70 THRCOF(I) = THRCOF(I) / SRHUGE
      SUM1 = SUM1 / HUGE
      SUMFOR = SUMFOR / HUGE
      X = X / SRHUGE
C
C
C  As long as ABS(C1) is decreasing, the recursion proceeds towards
C  increasing 3j values and, hence, is numerically stable.  Once
C  an increase of ABS(C1) is detected, the recursion direction is
C  reversed.
C
   80 IF(C1OLD-ABS(C1))   100, 100, 30
C
C
C  Keep three 3j coefficients around MMATCH for comparison later
C  with backward recursion values.
C
  100 CONTINUE
C     MMATCH = M2 - 1
      NSTEP2 = NFIN - LSTEP + 3
      X1 = X
      X2 = THRCOF(LSTEP-1)
      X3 = THRCOF(LSTEP-2)
C
C  Starting backward recursion from M2MAX taking NSTEP2 steps, so
C  that forwards and backwards recursion overlap at the three points
C  M2 = MMATCH+1, MMATCH, MMATCH-1.
C
      NFINP1 = NFIN + 1
      NFINP2 = NFIN + 2
      NFINP3 = NFIN + 3
      THRCOF(NFIN) = SRTINY
      SUM2 = TINY
C
C
C
      M2 = M2MAX + TWO
      LSTEP = 1
  110 LSTEP = LSTEP + 1
      M2 = M2 - ONE
      M3 = - M1 - M2
      OLDFAC = NEWFAC
      A1S = (L2-M2+TWO) * (L2+M2-ONE) * (L3+M3+TWO) * (L3-M3-ONE)
      NEWFAC = SQRT(A1S)
      DV = (L1+L2+L3+ONE)*(L2+L3-L1) - (L2-M2+ONE)*(L3+M3+ONE)
     1                               - (L2+M2-ONE)*(L3-M3-ONE)
      C1 = - DV / NEWFAC
      IF(LSTEP.GT.2)   GO TO 120
C
C  If M2 = M2MAX + 1 the third term in the recursion equation vanishes
C
      Y = SRTINY * C1
      THRCOF(NFIN-1) = Y
      IF(LSTEP.EQ.NSTEP2)   GO TO 200
      SUMBAC = SUM2
      SUM2 = SUM2 + Y*Y
      GO TO 110
C
  120 C2 = - OLDFAC / NEWFAC
C
C  Recursion to the next 3j coefficient
C
      Y = C1 * THRCOF(NFINP2-LSTEP) + C2 * THRCOF(NFINP3-LSTEP)
C
      IF(LSTEP.EQ.NSTEP2)   GO TO 200
C
      THRCOF(NFINP1-LSTEP) = Y
      SUMBAC = SUM2
      SUM2 = SUM2 + Y*Y
C
C
C  See if last 3j coefficient exceeds SRHUGE
C
      IF(ABS(Y).LT.SRHUGE)   GO TO 110
C
C  This is reached if last 3j coefficient larger than SRHUGE,
C  so that the recursion series THRCOF(NFIN), ... , THRCOF(NFIN-LSTEP+1)
C  has to be rescaled to prevent overflow.
C
C     MSCALE = MSCALE + 1
      DO 111 I=1,LSTEP
      INDEX = NFIN - I + 1
      IF(ABS(THRCOF(INDEX)).LT.SRTINY)
     1  THRCOF(INDEX) = ZERO
  111 THRCOF(INDEX) = THRCOF(INDEX) / SRHUGE
      SUM2 = SUM2 / HUGE
      SUMBAC = SUMBAC / HUGE
C
      GO TO 110
C
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
  230 CNORM = ONE / SQRT((L1+L1+ONE) * SUMUNI)
C
C  Sign convention for last 3j coefficient determines overall phase
C
      SIGN1 = SIGN(ONE,THRCOF(NFIN))
      SIGN2 = (-ONE) ** INT(ABS(L2-L3-M1)+EPS)
      IF(SIGN1*SIGN2)  235,235,236
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
C
C
      RETURN
      END
