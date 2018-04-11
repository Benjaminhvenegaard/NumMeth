      SUBROUTINE DPCHCE (IC, VC, N, X, H, SLOPE, D, INCFD, IERR)
C
C  Programming notes:
C     1. The function DPCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. One could reduce the number of arguments and amount of local
C        storage, at the expense of reduced code clarity, by passing in
C        the array WK (rather than splitting it into H and SLOPE) and
C        increasing its length enough to incorporate STEMP and XTEMP.
C     3. The two monotonicity checks only use the sufficient conditions.
C        Thus, it is possible (but unlikely) for a boundary condition to
C        be changed, even though the original interpolant was monotonic.
C        (At least the result is a continuous function of the data.)
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  IC(2), N, INCFD, IERR
      DOUBLE PRECISION  VC(2), X(*), H(*), SLOPE(*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  IBEG, IEND, IERF, INDEX, J, K
      DOUBLE PRECISION  HALF, STEMP(3), THREE, TWO, XTEMP(4), ZERO
      SAVE ZERO, HALF, TWO, THREE
      DOUBLE PRECISION  DPCHDF, DPCHST
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/,  HALF/.5D0/,  TWO/2.D0/, THREE/3.D0/
C
C***FIRST EXECUTABLE STATEMENT  DPCHCE
      IBEG = IC(1)
      IEND = IC(2)
      IERR = 0
C
C  SET TO DEFAULT BOUNDARY CONDITIONS IF N IS TOO SMALL.
C
      IF ( ABS(IBEG).GT.N )  IBEG = 0
      IF ( ABS(IEND).GT.N )  IEND = 0
C
C  TREAT BEGINNING BOUNDARY CONDITION.
C
      IF (IBEG .EQ. 0)  GO TO 2000
      K = ABS(IBEG)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,1) = VC(1)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,1) = HALF*( (THREE*SLOPE(1) - D(1,2)) - HALF*VC(1)*H(1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP FIRST K POINTS, IN REVERSE ORDER.
         DO 10  J = 1, K
            INDEX = K-J+1
C           INDEX RUNS FROM K DOWN TO 1.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX-1)
   10    CONTINUE
C                 -----------------------------
         D(1,1) = DPCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,1) = ( THREE*(H(1)*SLOPE(2) + H(2)*SLOPE(1))
     *             - TWO*(H(1)+H(2))*D(1,2) - H(1)*D(1,3) ) / H(2)
      ENDIF
C
      IF (IBEG .GT. 0)  GO TO 2000
C
C  CHECK D(1,1) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(1) .EQ. ZERO)  THEN
         IF (D(1,1) .NE. ZERO)  THEN
            D(1,1) = ZERO
            IERR = IERR + 1
         ENDIF
      ELSE IF ( DPCHST(D(1,1),SLOPE(1)) .LT. ZERO)  THEN
         D(1,1) = ZERO
         IERR = IERR + 1
      ELSE IF ( ABS(D(1,1)) .GT. THREE*ABS(SLOPE(1)) )  THEN
         D(1,1) = THREE*SLOPE(1)
         IERR = IERR + 1
      ENDIF
C
C  TREAT END BOUNDARY CONDITION.
C
 2000 CONTINUE
      IF (IEND .EQ. 0)  GO TO 5000
      K = ABS(IEND)
      IF (K .EQ. 1)  THEN
C        BOUNDARY VALUE PROVIDED.
         D(1,N) = VC(2)
      ELSE IF (K .EQ. 2)  THEN
C        BOUNDARY SECOND DERIVATIVE PROVIDED.
         D(1,N) = HALF*( (THREE*SLOPE(N-1) - D(1,N-1)) +
     *                                           HALF*VC(2)*H(N-1) )
      ELSE IF (K .LT. 5)  THEN
C        USE K-POINT DERIVATIVE FORMULA.
C        PICK UP LAST K POINTS.
         DO 2010  J = 1, K
            INDEX = N-K+J
C           INDEX RUNS FROM N+1-K UP TO N.
            XTEMP(J) = X(INDEX)
            IF (J .LT. K)  STEMP(J) = SLOPE(INDEX)
 2010    CONTINUE
C                 -----------------------------
         D(1,N) = DPCHDF (K, XTEMP, STEMP, IERF)
C                 -----------------------------
         IF (IERF .NE. 0)  GO TO 5001
      ELSE
C        USE 'NOT A KNOT' CONDITION.
         D(1,N) = ( THREE*(H(N-1)*SLOPE(N-2) + H(N-2)*SLOPE(N-1))
     *             - TWO*(H(N-1)+H(N-2))*D(1,N-1) - H(N-1)*D(1,N-2) )
     *                                                         / H(N-2)
      ENDIF
C
      IF (IEND .GT. 0)  GO TO 5000
C
C  CHECK D(1,N) FOR COMPATIBILITY WITH MONOTONICITY.
C
      IF (SLOPE(N-1) .EQ. ZERO)  THEN
         IF (D(1,N) .NE. ZERO)  THEN
            D(1,N) = ZERO
            IERR = IERR + 2
         ENDIF
      ELSE IF ( DPCHST(D(1,N),SLOPE(N-1)) .LT. ZERO)  THEN
         D(1,N) = ZERO
         IERR = IERR + 2
      ELSE IF ( ABS(D(1,N)) .GT. THREE*ABS(SLOPE(N-1)) )  THEN
         D(1,N) = THREE*SLOPE(N-1)
         IERR = IERR + 2
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURN.
C
 5001 CONTINUE
C     ERROR RETURN FROM DPCHDF.
C   *** THIS CASE SHOULD NEVER OCCUR ***
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHCE', 'ERROR RETURN FROM DPCHDF',
     +   IERR, 1)
      RETURN
C------------- LAST LINE OF DPCHCE FOLLOWS -----------------------------
      END
