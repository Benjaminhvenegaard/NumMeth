      SUBROUTINE DNLS1 (FCN, IOPT, M, N, X, FVEC, FJAC, LDFJAC, FTOL,
     +   XTOL, GTOL, MAXFEV, EPSFCN, DIAG, MODE, FACTOR, NPRINT, INFO,
     +   NFEV, NJEV, IPVT, QTF, WA1, WA2, WA3, WA4)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IOPT,M,N,LDFJAC,MAXFEV,MODE,NPRINT,INFO,NFEV,NJEV
      INTEGER IJUNK,NROW,IPVT(*)
      DOUBLE PRECISION FTOL,XTOL,GTOL,FACTOR,EPSFCN
      DOUBLE PRECISION X(*),FVEC(*),FJAC(LDFJAC,*),DIAG(*),QTF(*),
     1     WA1(*),WA2(*),WA3(*),WA4(*)
      LOGICAL SING
      EXTERNAL FCN
      INTEGER I,IFLAG,ITER,J,L,MODECH
      DOUBLE PRECISION ACTRED,DELTA,DIRDER,EPSMCH,FNORM,FNORM1,GNORM,
     1     ONE,PAR,PNORM,PRERED,P1,P5,P25,P75,P0001,RATIO,SUM,TEMP,
     2     TEMP1,TEMP2,XNORM,ZERO
      DOUBLE PRECISION D1MACH,DENORM,ERR,CHKLIM
      CHARACTER*8 XERN1
      CHARACTER*16 XERN3
      SAVE CHKLIM, ONE, P1, P5, P25, P75, P0001, ZERO
C
      DATA CHKLIM/.1D0/
      DATA ONE,P1,P5,P25,P75,P0001,ZERO
     1     /1.0D0,1.0D-1,5.0D-1,2.5D-1,7.5D-1,1.0D-4,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DNLS1
      EPSMCH = D1MACH(4)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C
C     CHECK THE INPUT PARAMETERS FOR ERRORS.
C
      IF (IOPT .LT. 1 .OR. IOPT .GT. 3 .OR. N .LE. 0 .OR.
     1    M .LT. N .OR. LDFJAC .LT. N .OR. FTOL .LT. ZERO
     2    .OR. XTOL .LT. ZERO .OR. GTOL .LT. ZERO
     3    .OR. MAXFEV .LE. 0 .OR. FACTOR .LE. ZERO) GO TO 300
      IF (IOPT .LT. 3 .AND. LDFJAC .LT. M) GO TO 300
      IF (MODE .NE. 2) GO TO 20
      DO 10 J = 1, N
         IF (DIAG(J) .LE. ZERO) GO TO 300
   10    CONTINUE
   20 CONTINUE
C
C     EVALUATE THE FUNCTION AT THE STARTING POINT
C     AND CALCULATE ITS NORM.
C
      IFLAG = 1
      IJUNK = 1
      CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
      NFEV = 1
      IF (IFLAG .LT. 0) GO TO 300
      FNORM = DENORM(M,FVEC)
C
C     INITIALIZE LEVENBERG-MARQUARDT PARAMETER AND ITERATION COUNTER.
C
      PAR = ZERO
      ITER = 1
C
C     BEGINNING OF THE OUTER LOOP.
C
   30 CONTINUE
C
C        IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.
C
         IF (NPRINT .LE. 0) GO TO 40
         IFLAG = 0
         IF (MOD(ITER-1,NPRINT) .EQ. 0)
     1      CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
         IF (IFLAG .LT. 0) GO TO 300
   40    CONTINUE
C
C        CALCULATE THE JACOBIAN MATRIX.
C
      IF (IOPT .EQ. 3) GO TO 475
C
C     STORE THE FULL JACOBIAN USING M*N STORAGE
C
      IF (IOPT .EQ. 1) GO TO 410
C
C     THE USER SUPPLIES THE JACOBIAN
C
         IFLAG = 2
         CALL FCN(IFLAG,M,N,X,FVEC,FJAC,LDFJAC)
         NJEV = NJEV + 1
C
C             ON THE FIRST ITERATION, CHECK THE USER SUPPLIED JACOBIAN
C
         IF (ITER .LE. 1) THEN
            IF (IFLAG .LT. 0) GO TO 300
C
C           GET THE INCREMENTED X-VALUES INTO WA1(*).
C
            MODECH = 1
            CALL DCKDER(M,N,X,FVEC,FJAC,LDFJAC,WA1,WA4,MODECH,ERR)
C
C           EVALUATE FUNCTION AT INCREMENTED VALUE AND PUT IN WA4(*).
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA1,WA4,FJAC,LDFJAC)
            NFEV = NFEV + 1
            IF(IFLAG .LT. 0) GO TO 300
            DO 350 I = 1, M
               MODECH = 2
               CALL DCKDER(1,N,X,FVEC(I),FJAC(I,1),LDFJAC,WA1,
     1              WA4(I),MODECH,ERR)
               IF (ERR .LT. CHKLIM) THEN
                  WRITE (XERN1, '(I8)') I
                  WRITE (XERN3, '(1PE15.6)') ERR
                  CALL XERMSG ('SLATEC', 'DNLS1', 'DERIVATIVE OF ' //
     *               'FUNCTION ' // XERN1 // ' MAY BE WRONG, ERR = ' //
     *               XERN3 // ' TOO CLOSE TO 0.', 7, 0)
               ENDIF
  350       CONTINUE
         ENDIF
C
         GO TO 420
C
C     THE CODE APPROXIMATES THE JACOBIAN
C
410      IFLAG = 1
         CALL DFDJC3(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN,WA4)
         NFEV = NFEV + N
  420    IF (IFLAG .LT. 0) GO TO 300
C
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.
C
         CALL DQRFAC(M,N,FJAC,LDFJAC,.TRUE.,IPVT,N,WA1,WA2,WA3)
C
C        FORM (Q TRANSPOSE)*FVEC AND STORE THE FIRST N COMPONENTS IN
C        QTF.
C
         DO 430 I = 1, M
            WA4(I) = FVEC(I)
  430         CONTINUE
         DO 470 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 460
            SUM = ZERO
            DO 440 I = J, M
               SUM = SUM + FJAC(I,J)*WA4(I)
  440          CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 450 I = J, M
               WA4(I) = WA4(I) + FJAC(I,J)*TEMP
  450          CONTINUE
  460       CONTINUE
            FJAC(J,J) = WA1(J)
            QTF(J) = WA4(J)
  470       CONTINUE
         GO TO 560
C
C        ACCUMULATE THE JACOBIAN BY ROWS IN ORDER TO SAVE STORAGE.
C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN MATRIX
C        CALCULATED ONE ROW AT A TIME, WHILE SIMULTANEOUSLY
C        FORMING (Q TRANSPOSE)*FVEC AND STORING THE FIRST
C        N COMPONENTS IN QTF.
C
  475    DO 490 J = 1, N
            QTF(J) = ZERO
            DO 480 I = 1, N
               FJAC(I,J) = ZERO
  480          CONTINUE
  490        CONTINUE
         DO 500 I = 1, M
            NROW = I
            IFLAG = 3
            CALL FCN(IFLAG,M,N,X,FVEC,WA3,NROW)
            IF (IFLAG .LT. 0) GO TO 300
C
C            ON THE FIRST ITERATION, CHECK THE USER SUPPLIED JACOBIAN.
C
            IF(ITER .GT. 1) GO TO 498
C
C            GET THE INCREMENTED X-VALUES INTO WA1(*).
C
            MODECH = 1
            CALL DCKDER(M,N,X,FVEC,FJAC,LDFJAC,WA1,WA4,MODECH,ERR)
C
C            EVALUATE AT INCREMENTED VALUES, IF NOT ALREADY EVALUATED.
C
            IF(I .NE. 1) GO TO 495
C
C            EVALUATE FUNCTION AT INCREMENTED VALUE AND PUT INTO WA4(*).
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA1,WA4,FJAC,NROW)
            NFEV = NFEV + 1
            IF(IFLAG .LT. 0) GO TO 300
495         CONTINUE
            MODECH = 2
            CALL DCKDER(1,N,X,FVEC(I),WA3,1,WA1,WA4(I),MODECH,ERR)
            IF (ERR .LT. CHKLIM) THEN
               WRITE (XERN1, '(I8)') I
               WRITE (XERN3, '(1PE15.6)') ERR
               CALL XERMSG ('SLATEC', 'DNLS1', 'DERIVATIVE OF FUNCTION '
     *            // XERN1 // ' MAY BE WRONG, ERR = ' // XERN3 //
     *            ' TOO CLOSE TO 0.', 7, 0)
            ENDIF
498         CONTINUE
C
            TEMP = FVEC(I)
            CALL DWUPDT(N,FJAC,LDFJAC,WA3,QTF,TEMP,WA1,WA2)
  500       CONTINUE
         NJEV = NJEV + 1
C
C        IF THE JACOBIAN IS RANK DEFICIENT, CALL DQRFAC TO
C        REORDER ITS COLUMNS AND UPDATE THE COMPONENTS OF QTF.
C
         SING = .FALSE.
         DO 510 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) SING = .TRUE.
            IPVT(J) = J
            WA2(J) = DENORM(J,FJAC(1,J))
  510       CONTINUE
         IF (.NOT.SING) GO TO 560
         CALL DQRFAC(N,N,FJAC,LDFJAC,.TRUE.,IPVT,N,WA1,WA2,WA3)
         DO 550 J = 1, N
            IF (FJAC(J,J) .EQ. ZERO) GO TO 540
            SUM = ZERO
            DO 520 I = J, N
               SUM = SUM + FJAC(I,J)*QTF(I)
  520         CONTINUE
            TEMP = -SUM/FJAC(J,J)
            DO 530 I = J, N
               QTF(I) = QTF(I) + FJAC(I,J)*TEMP
  530          CONTINUE
  540       CONTINUE
            FJAC(J,J) = WA1(J)
  550       CONTINUE
  560    CONTINUE
C
C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING
C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.
C
         IF (ITER .NE. 1) GO TO 80
         IF (MODE .EQ. 2) GO TO 60
         DO 50 J = 1, N
            DIAG(J) = WA2(J)
            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE
   50       CONTINUE
   60    CONTINUE
C
C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X
C        AND INITIALIZE THE STEP BOUND DELTA.
C
         DO 70 J = 1, N
            WA3(J) = DIAG(J)*X(J)
   70       CONTINUE
         XNORM = DENORM(N,WA3)
         DELTA = FACTOR*XNORM
         IF (DELTA .EQ. ZERO) DELTA = FACTOR
   80    CONTINUE
C
C        COMPUTE THE NORM OF THE SCALED GRADIENT.
C
         GNORM = ZERO
         IF (FNORM .EQ. ZERO) GO TO 170
         DO 160 J = 1, N
            L = IPVT(J)
            IF (WA2(L) .EQ. ZERO) GO TO 150
            SUM = ZERO
            DO 140 I = 1, J
               SUM = SUM + FJAC(I,J)*(QTF(I)/FNORM)
  140          CONTINUE
            GNORM = MAX(GNORM,ABS(SUM/WA2(L)))
  150       CONTINUE
  160       CONTINUE
  170    CONTINUE
C
C        TEST FOR CONVERGENCE OF THE GRADIENT NORM.
C
         IF (GNORM .LE. GTOL) INFO = 4
         IF (INFO .NE. 0) GO TO 300
C
C        RESCALE IF NECESSARY.
C
         IF (MODE .EQ. 2) GO TO 190
         DO 180 J = 1, N
            DIAG(J) = MAX(DIAG(J),WA2(J))
  180       CONTINUE
  190    CONTINUE
C
C        BEGINNING OF THE INNER LOOP.
C
  200    CONTINUE
C
C           DETERMINE THE LEVENBERG-MARQUARDT PARAMETER.
C
            CALL DMPAR(N,FJAC,LDFJAC,IPVT,DIAG,QTF,DELTA,PAR,WA1,WA2,
     1                 WA3,WA4)
C
C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.
C
            DO 210 J = 1, N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
  210          CONTINUE
            PNORM = DENORM(N,WA3)
C
C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.
C
            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)
C
C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.
C
            IFLAG = 1
            CALL FCN(IFLAG,M,N,WA2,WA4,FJAC,IJUNK)
            NFEV = NFEV + 1
            IF (IFLAG .LT. 0) GO TO 300
            FNORM1 = DENORM(M,WA4)
C
C           COMPUTE THE SCALED ACTUAL REDUCTION.
C
            ACTRED = -ONE
            IF (P1*FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C
C           COMPUTE THE SCALED PREDICTED REDUCTION AND
C           THE SCALED DIRECTIONAL DERIVATIVE.
C
            DO 230 J = 1, N
               WA3(J) = ZERO
               L = IPVT(J)
               TEMP = WA1(L)
               DO 220 I = 1, J
                  WA3(I) = WA3(I) + FJAC(I,J)*TEMP
  220             CONTINUE
  230          CONTINUE
            TEMP1 = DENORM(N,WA3)/FNORM
            TEMP2 = (SQRT(PAR)*PNORM)/FNORM
            PRERED = TEMP1**2 + TEMP2**2/P5
            DIRDER = -(TEMP1**2 + TEMP2**2)
C
C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED
C           REDUCTION.
C
            RATIO = ZERO
            IF (PRERED .NE. ZERO) RATIO = ACTRED/PRERED
C
C           UPDATE THE STEP BOUND.
C
            IF (RATIO .GT. P25) GO TO 240
               IF (ACTRED .GE. ZERO) TEMP = P5
               IF (ACTRED .LT. ZERO)
     1            TEMP = P5*DIRDER/(DIRDER + P5*ACTRED)
               IF (P1*FNORM1 .GE. FNORM .OR. TEMP .LT. P1) TEMP = P1
               DELTA = TEMP*MIN(DELTA,PNORM/P1)
               PAR = PAR/TEMP
               GO TO 260
  240       CONTINUE
               IF (PAR .NE. ZERO .AND. RATIO .LT. P75) GO TO 250
               DELTA = PNORM/P5
               PAR = P5*PAR
  250          CONTINUE
  260       CONTINUE
C
C           TEST FOR SUCCESSFUL ITERATION.
C
            IF (RATIO .LT. P0001) GO TO 290
C
C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.
C
            DO 270 J = 1, N
               X(J) = WA2(J)
               WA2(J) = DIAG(J)*X(J)
  270          CONTINUE
            DO 280 I = 1, M
               FVEC(I) = WA4(I)
  280          CONTINUE
            XNORM = DENORM(N,WA2)
            FNORM = FNORM1
            ITER = ITER + 1
  290       CONTINUE
C
C           TESTS FOR CONVERGENCE.
C
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     1          .AND. P5*RATIO .LE. ONE) INFO = 1
            IF (DELTA .LE. XTOL*XNORM) INFO = 2
            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL
     1          .AND. P5*RATIO .LE. ONE .AND. INFO .EQ. 2) INFO = 3
            IF (INFO .NE. 0) GO TO 300
C
C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.
C
            IF (NFEV .GE. MAXFEV) INFO = 5
            IF (ABS(ACTRED) .LE. EPSMCH .AND. PRERED .LE. EPSMCH
     1          .AND. P5*RATIO .LE. ONE) INFO = 6
            IF (DELTA .LE. EPSMCH*XNORM) INFO = 7
            IF (GNORM .LE. EPSMCH) INFO = 8
            IF (INFO .NE. 0) GO TO 300
C
C           END OF THE INNER LOOP. REPEAT IF ITERATION UNSUCCESSFUL.
C
            IF (RATIO .LT. P0001) GO TO 200
C
C        END OF THE OUTER LOOP.
C
         GO TO 30
  300 CONTINUE
C
C     TERMINATION, EITHER NORMAL OR USER IMPOSED.
C
      IF (IFLAG .LT. 0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT .GT. 0) CALL FCN(IFLAG,M,N,X,FVEC,FJAC,IJUNK)
      IF (INFO .LT. 0) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'EXECUTION TERMINATED BECAUSE USER SET IFLAG NEGATIVE.', 1, 1)
      IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'INVALID INPUT PARAMETER.', 2, 1)
      IF (INFO .EQ. 4) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'THIRD CONVERGENCE CONDITION, CHECK RESULTS BEFORE ACCEPTING.',
     +   1, 1)
      IF (INFO .EQ. 5) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'TOO MANY FUNCTION EVALUATIONS.', 9, 1)
      IF (INFO .GE. 6) CALL XERMSG ('SLATEC', 'DNLS1',
     +   'TOLERANCES TOO SMALL, NO FURTHER IMPROVEMENT POSSIBLE.', 3, 1)
      RETURN
C
C     LAST CARD OF SUBROUTINE DNLS1.
C
      END