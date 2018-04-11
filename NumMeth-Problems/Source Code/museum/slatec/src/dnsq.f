      SUBROUTINE DNSQ (FCN, JAC, IOPT, N, X, FVEC, FJAC, LDFJAC, XTOL,
     +   MAXFEV, ML, MU, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV,
     +   NJEV, R, LR, QTF, WA1, WA2, WA3, WA4)
      DOUBLE PRECISION D1MACH,DENORM
      INTEGER I, IFLAG, INFO, IOPT, ITER, IWA(1), J, JM1, L, LDFJAC,
     1     LR, MAXFEV, ML, MODE, MU, N, NCFAIL, NCSUC, NFEV, NJEV,
     2     NPRINT, NSLOW1, NSLOW2
      DOUBLE PRECISION ACTRED, DELTA, DIAG(*), EPSFCN, EPSMCH, FACTOR,
     1     FJAC(LDFJAC,*), FNORM, FNORM1, FVEC(*), ONE, P0001, P001,
     2     P1, P5, PNORM, PRERED, QTF(*), R(*), RATIO, SUM, TEMP,
     3     WA1(*), WA2(*), WA3(*), WA4(*), X(*), XNORM, XTOL, ZERO
      EXTERNAL FCN
      LOGICAL JEVAL,SING
      SAVE ONE, P1, P5, P001, P0001, ZERO
      DATA ONE,P1,P5,P001,P0001,ZERO
     1     /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/
C
C     BEGIN BLOCK PERMITTING ...EXITS TO 320
C***FIRST EXECUTABLE STATEMENT  DNSQ
         EPSMCH = D1MACH(4)
C
         INFO = 0
         IFLAG = 0
         NFEV = 0
         NJEV = 0
C
C        CHECK THE INPUT PARAMETERS FOR ERRORS.
C
C     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1       .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0 .OR. ML .LT. 0
     2       .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO .OR. LDFJAC .LT. N
     3       .OR. LR .LT. (N*(N + 1))/2) GO TO 320
         IF (MODE .NE. 2) GO TO 20
            DO 10 J = 1, N
C     .........EXIT
               IF (DIAG(J) .LE. ZERO) GO TO 320
   10       CONTINUE
   20    CONTINUE
C
C        EVALUATE THE FUNCTION AT THE STARTING POINT
C        AND CALCULATE ITS NORM.
C
         IFLAG = 1
         CALL FCN(N,X,FVEC,IFLAG)
         NFEV = 1
C     ...EXIT
         IF (IFLAG .LT. 0) GO TO 320
         FNORM = DENORM(N,FVEC)
C
C        INITIALIZE ITERATION COUNTER AND MONITORS.
C
         ITER = 1
         NCSUC = 0
         NCFAIL = 0
         NSLOW1 = 0
         NSLOW2 = 0
C
C        BEGINNING OF THE OUTER LOOP.
C
   30    CONTINUE
C           BEGIN BLOCK PERMITTING ...EXITS TO 90
               JEVAL = .TRUE.
C
C              CALCULATE THE JACOBIAN MATRIX.
C
               IF (IOPT .EQ. 2) GO TO 40
C
C                 USER SUPPLIES JACOBIAN
C
                  CALL JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
                  NJEV = NJEV + 1
               GO TO 50
   40          CONTINUE
C
C                 CODE APPROXIMATES THE JACOBIAN
C
                  IFLAG = 2
                  CALL DFDJC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,
     1                        EPSFCN,WA1,WA2)
                  NFEV = NFEV + MIN(ML+MU+1,N)
   50          CONTINUE
C
C     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
C
C              COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
               CALL DQRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)
C
C              ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C              TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
C           ...EXIT
               IF (ITER .NE. 1) GO TO 90
               IF (MODE .EQ. 2) GO TO 70
                  DO 60 J = 1, N
                     DIAG(J) = WA2(J)
                     IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   60             CONTINUE
   70          CONTINUE
C
C              ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED
C              X AND INITIALIZE THE STEP BOUND DELTA.
C
               DO 80 J = 1, N
                  WA3(J) = DIAG(J)*X(J)
   80          CONTINUE
               XNORM = DENORM(N,WA3)
               DELTA = FACTOR*XNORM
               IF (DELTA .EQ. ZERO) DELTA = FACTOR
   90       CONTINUE
C
C           FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.
C
            DO 100 I = 1, N
               QTF(I) = FVEC(I)
  100       CONTINUE
            DO 140 J = 1, N
               IF (FJAC(J,J) .EQ. ZERO) GO TO 130
                  SUM = ZERO
                  DO 110 I = J, N
                     SUM = SUM + FJAC(I,J)*QTF(I)
  110             CONTINUE
                  TEMP = -SUM/FJAC(J,J)
                  DO 120 I = J, N
                     QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  120             CONTINUE
  130          CONTINUE
  140       CONTINUE
C
C           COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.
C
            SING = .FALSE.
            DO 170 J = 1, N
               L = J
               JM1 = J - 1
               IF (JM1 .LT. 1) GO TO 160
               DO 150 I = 1, JM1
                  R(L) = FJAC(I,J)
                  L = L + N - I
  150          CONTINUE
  160          CONTINUE
               R(L) = WA1(J)
               IF (WA1(J) .EQ. ZERO) SING = .TRUE.
  170       CONTINUE
C
C           ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.
C
            CALL DQFORM(N,N,FJAC,LDFJAC,WA1)
C
C           RESCALE IF NECESSARY.
C
            IF (MODE .EQ. 2) GO TO 190
               DO 180 J = 1, N
                  DIAG(J) = MAX(DIAG(J),WA2(J))
  180          CONTINUE
  190       CONTINUE
C
C           BEGINNING OF THE INNER LOOP.
C
  200       CONTINUE
C
C              IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
               IF (NPRINT .LE. 0) GO TO 210
                  IFLAG = 0
                  IF (MOD(ITER-1,NPRINT) .EQ. 0)
     1               CALL FCN(N,X,FVEC,IFLAG)
C     ............EXIT
                  IF (IFLAG .LT. 0) GO TO 320
  210          CONTINUE
C
C              DETERMINE THE DIRECTION P.
C
               CALL DDOGLG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)
C
C              STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
               DO 220 J = 1, N
                  WA1(J) = -WA1(J)
                  WA2(J) = X(J) + WA1(J)
                  WA3(J) = DIAG(J)*WA1(J)
  220          CONTINUE
               PNORM = DENORM(N,WA3)
C
C              ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
               IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C              EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
               IFLAG = 1
               CALL FCN(N,WA2,WA4,IFLAG)
               NFEV = NFEV + 1
C     .........EXIT
               IF (IFLAG .LT. 0) GO TO 320
               FNORM1 = DENORM(N,WA4)
C
C              COMPUTE THE SCALED ACTUAL REDUCTION.
C
               ACTRED = -ONE
               IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C              COMPUTE THE SCALED PREDICTED REDUCTION.
C
               L = 1
               DO 240 I = 1, N
                  SUM = ZERO
                  DO 230 J = I, N
                     SUM = SUM + R(L)*WA1(J)
                     L = L + 1
  230             CONTINUE
                  WA3(I) = QTF(I) + SUM
  240          CONTINUE
               TEMP = DENORM(N,WA3)
               PRERED = ZERO
               IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2
C
C              COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C              REDUCTION.
C
               RATIO = ZERO
               IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED
C
C              UPDATE THE STEP BOUND.
C
               IF (RATIO .GE. P1) GO TO 250
                  NCSUC = 0
                  NCFAIL = NCFAIL + 1
                  DELTA = P5*DELTA
               GO TO 260
  250          CONTINUE
                  NCFAIL = 0
                  NCSUC = NCSUC + 1
                  IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)
     1               DELTA = MAX(DELTA,PNORM/P5)
                  IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5
  260          CONTINUE
C
C              TEST FOR SUCCESSFUL ITERATION.
C
               IF (RATIO .LT. P0001) GO TO 280
C
C                 SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
                  DO 270 J = 1, N
                     X(J) = WA2(J)
                     WA2(J) = DIAG(J)*X(J)
                     FVEC(J) = WA4(J)
  270             CONTINUE
                  XNORM = DENORM(N,WA2)
                  FNORM = FNORM1
                  ITER = ITER + 1
  280          CONTINUE
C
C              DETERMINE THE PROGRESS OF THE ITERATION.
C
               NSLOW1 = NSLOW1 + 1
               IF (ACTRED .GE. P001) NSLOW1 = 0
               IF (JEVAL) NSLOW2 = NSLOW2 + 1
               IF (ACTRED .GE. P1) NSLOW2 = 0
C
C              TEST FOR CONVERGENCE.
C
               IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1
C     .........EXIT
               IF (INFO .NE. 0) GO TO 320
C
C              TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
               IF (NFEV .GE. MAXFEV) INFO = 2
               IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3
               IF (NSLOW2 .EQ. 5) INFO = 4
               IF (NSLOW1 .EQ. 10) INFO = 5
C     .........EXIT
               IF (INFO .NE. 0) GO TO 320
C
C              CRITERION FOR RECALCULATING JACOBIAN
C
C           ...EXIT
               IF (NCFAIL .EQ. 2) GO TO 310
C
C              CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN
C              AND UPDATE QTF IF NECESSARY.
C
               DO 300 J = 1, N
                  SUM = ZERO
                  DO 290 I = 1, N
                     SUM = SUM + FJAC(I,J)*WA4(I)
  290             CONTINUE
                  WA2(J) = (SUM - WA3(J))/PNORM
                  WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)
                  IF (RATIO .GE. P0001) QTF(J) = SUM
  300          CONTINUE
C
C              COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.
C
               CALL D1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)
               CALL D1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)
               CALL D1MPYQ(1,N,QTF,1,WA2,WA3)
C
C              END OF THE INNER LOOP.
C
               JEVAL = .FALSE.
            GO TO 200
  310       CONTINUE
C
C           END OF THE OUTER LOOP.
C
         GO TO 30
  320 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG)
      IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'DNSQ',
     +   'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
      IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQ',
     +   'INVALID INPUT PARAMETER.', 2, 1)
      IF (INFO .EQ. 2) CALL XERMSG ('SLATEC', 'DNSQ',
     +   'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
      IF (INFO .EQ. 3) CALL XERMSG ('SLATEC', 'DNSQ',
     +   'XTOL TOO SMALL. NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
      IF (INFO .GT. 4) CALL XERMSG ('SLATEC', 'DNSQ',
     +   'ITERATION NOT MAKING GOOD PROGRESS.', 1, 1)
      RETURN
C
C     LAST CARD OF SUBROUTINE DNSQ.
C
      END
