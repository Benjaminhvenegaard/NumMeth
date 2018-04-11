      SUBROUTINE CHFDV (X1, X2, F1, F2, D1, D2, NE, XE, FE, DE, NEXT,
     +   IERR)
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change CHFDV to DCHFDV wherever it occurs,
C        b. Change the real declaration to double precision, and
C        c. Change the constant ZERO to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  NE, NEXT(2), IERR
      REAL  X1, X2, F1, F2, D1, D2, XE(*), FE(*), DE(*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I
      REAL  C2, C2T2, C3, C3T3, DEL1, DEL2, DELTA, H, X, XMI, XMA, ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  CHFDV
      IF (NE .LT. 1)  GO TO 5001
      H = X2 - X1
      IF (H .EQ. ZERO)  GO TO 5002
C
C  INITIALIZE.
C
      IERR = 0
      NEXT(1) = 0
      NEXT(2) = 0
      XMI = MIN(ZERO, H)
      XMA = MAX(ZERO, H)
C
C  COMPUTE CUBIC COEFFICIENTS (EXPANDED ABOUT X1).
C
      DELTA = (F2 - F1)/H
      DEL1 = (D1 - DELTA)/H
      DEL2 = (D2 - DELTA)/H
C                                           (DELTA IS NO LONGER NEEDED.)
      C2 = -(DEL1+DEL1 + DEL2)
      C2T2 = C2 + C2
      C3 = (DEL1 + DEL2)/H
C                               (H, DEL1 AND DEL2 ARE NO LONGER NEEDED.)
      C3T3 = C3+C3+C3
C
C  EVALUATION LOOP.
C
      DO 500  I = 1, NE
         X = XE(I) - X1
         FE(I) = F1 + X*(D1 + X*(C2 + X*C3))
         DE(I) = D1 + X*(C2T2 + X*C3T3)
C          COUNT EXTRAPOLATION POINTS.
         IF ( X.LT.XMI )  NEXT(1) = NEXT(1) + 1
         IF ( X.GT.XMA )  NEXT(2) = NEXT(2) + 1
C        (NOTE REDUNDANCY--IF EITHER CONDITION IS TRUE, OTHER IS FALSE.)
  500 CONTINUE
C
C  NORMAL RETURN.
C
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     NE.LT.1 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'CHFDV',
     +   'NUMBER OF EVALUATION POINTS LESS THAN ONE', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     X1.EQ.X2 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'CHFDV', 'INTERVAL ENDPOINTS EQUAL', IERR,
     +   1)
      RETURN
C------------- LAST LINE OF CHFDV FOLLOWS ------------------------------
      END
