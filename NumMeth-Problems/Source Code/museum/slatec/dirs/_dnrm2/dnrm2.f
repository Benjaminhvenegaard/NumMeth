      DOUBLE PRECISION FUNCTION DNRM2 (N, DX, INCX)
      INTEGER NEXT
      DOUBLE PRECISION DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX, ZERO,
     +                 ONE
      SAVE CUTLO, CUTHI, ZERO, ONE
      DATA ZERO, ONE /0.0D0, 1.0D0/
C
      DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
C***FIRST EXECUTABLE STATEMENT  DNRM2
      IF (N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N * INCX
C
C                                                 BEGIN MAIN LOOP
C
      I = 1
   20    GO TO NEXT,(30, 50, 70, 110)
   30 IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
      ASSIGN 50 TO NEXT
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF (DX(I) .EQ. ZERO) GO TO 200
      IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
C
      ASSIGN 70 TO NEXT
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
      ASSIGN 110 TO NEXT
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = ABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF (ABS(DX(I)) .GT. CUTLO) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF (ABS(DX(I)) .LE. XMAX) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = ABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 HITEST = CUTHI / N
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J = I,NN,INCX
      IF (ABS(DX(J)) .GE. HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      DNRM2 = SQRT(SUM)
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF (I .LE. NN) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * SQRT(SUM)
  300 CONTINUE
      RETURN
      END
