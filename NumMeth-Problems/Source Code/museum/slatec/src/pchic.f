      SUBROUTINE PCHIC (IC, VC, SWITCH, N, X, F, D, INCFD, WK, NWK,
     +   IERR)
C  Programming notes:
C
C     To produce a double precision version, simply:
C        a. Change PCHIC to DPCHIC wherever it occurs,
C        b. Change PCHCE to DPCHCE wherever it occurs,
C        c. Change PCHCI to DPCHCI wherever it occurs,
C        d. Change PCHCS to DPCHCS wherever it occurs,
C        e. Change the real declarations to double precision, and
C        f. Change the constant  ZERO  to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, NWK, IERR
      REAL  VC(2), SWITCH, X(*), F(INCFD,*), D(INCFD,*), WK(NWK)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IBEG, IEND, NLESS1
      REAL  ZERO
      SAVE ZERO
      DATA  ZERO /0./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHIC
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
      IF (ABS(IBEG) .GT. 5)  IERR = IERR - 1
      IF (ABS(IEND) .GT. 5)  IERR = IERR - 2
      IF (IERR .LT. 0)  GO TO 5004
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
      NLESS1 = N - 1
      IF ( NWK .LT. 2*NLESS1 )  GO TO 5007
C
C  SET UP H AND SLOPE ARRAYS.
C
      DO 20  I = 1, NLESS1
         WK(I) = X(I+1) - X(I)
         WK(NLESS1+I) = (F(1,I+1) - F(1,I)) / WK(I)
   20 CONTINUE
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 1000
      D(1,1) = WK(2)
      D(1,N) = WK(2)
      GO TO 3000
C
C  NORMAL CASE  (N .GE. 3) .
C
 1000 CONTINUE
C
C  SET INTERIOR DERIVATIVES AND DEFAULT END CONDITIONS.
C
C     --------------------------------------
      CALL PCHCI (N, WK(1), WK(N), D, INCFD)
C     --------------------------------------
C
C  SET DERIVATIVES AT POINTS WHERE MONOTONICITY SWITCHES DIRECTION.
C
      IF (SWITCH .EQ. ZERO)  GO TO 3000
C     ----------------------------------------------------
      CALL PCHCS (SWITCH, N, WK(1), WK(N), D, INCFD, IERR)
C     ----------------------------------------------------
      IF (IERR .NE. 0)  GO TO 5008
C
C  SET END CONDITIONS.
C
 3000 CONTINUE
      IF ( (IBEG.EQ.0) .AND. (IEND.EQ.0) )  GO TO 5000
C     -------------------------------------------------------
      CALL PCHCE (IC, VC, N, X, WK(1), WK(N), D, INCFD, IERR)
C     -------------------------------------------------------
      IF (IERR .LT. 0)  GO TO 5009
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHIC',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHIC', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHIC', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C
 5004 CONTINUE
C     IC OUT OF RANGE RETURN.
      IERR = IERR - 3
      CALL XERMSG ('SLATEC', 'PCHIC', 'IC OUT OF RANGE', IERR, 1)
      RETURN
C
 5007 CONTINUE
C     NWK .LT. 2*(N-1)  RETURN.
      IERR = -7
      CALL XERMSG ('SLATEC', 'PCHIC', 'WORK ARRAY TOO SMALL', IERR, 1)
      RETURN
C
 5008 CONTINUE
C     ERROR RETURN FROM PCHCS.
      IERR = -8
      CALL XERMSG ('SLATEC', 'PCHIC', 'ERROR RETURN FROM PCHCS', IERR,
     +   1)
      RETURN
C
 5009 CONTINUE
C     ERROR RETURN FROM PCHCE.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -9
      CALL XERMSG ('SLATEC', 'PCHIC', 'ERROR RETURN FROM PCHCE', IERR,
     +   1)
      RETURN
C------------- LAST LINE OF PCHIC FOLLOWS ------------------------------
      END
