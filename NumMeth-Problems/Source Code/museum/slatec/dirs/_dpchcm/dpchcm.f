      SUBROUTINE DPCHCM (N, X, F, D, INCFD, SKIP, ISMON, IERR)
C
C  Fortran intrinsics used:  ISIGN.
C  Other routines used:  CHFCM, XERMSG.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     An alternate organization would have separate loops for computing
C     ISMON(i), i=1,...,NSEG, and for the computation of ISMON(N).  The
C     first loop can be readily parallelized, since the NSEG calls to
C     CHFCM are independent.  The second loop can be cut short if
C     ISMON(N) is ever equal to 2, for it cannot be changed further.
C
C     To produce a single precision version, simply:
C        a. Change DPCHCM to PCHCM wherever it occurs,
C        b. Change DCHFCM to CHFCM wherever it occurs, and
C        c. Change the double precision declarations to real.
C
C  DECLARE ARGUMENTS.
C
      INTEGER N, INCFD, ISMON(N), IERR
      DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER I, NSEG
      DOUBLE PRECISION  DELTA
      INTEGER DCHFCM
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  DPCHCM
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
      SKIP = .TRUE.
C
C  FUNCTION DEFINITION IS OK -- GO ON.
C
    5 CONTINUE
      NSEG = N - 1
      DO 90  I = 1, NSEG
         DELTA = (F(1,I+1)-F(1,I))/(X(I+1)-X(I))
C                   -------------------------------
         ISMON(I) = DCHFCM (D(1,I), D(1,I+1), DELTA)
C                   -------------------------------
         IF (I .EQ. 1)  THEN
            ISMON(N) = ISMON(1)
         ELSE
C           Need to figure out cumulative monotonicity from following
C           "multiplication table":
C
C                    +        I S M O N (I)
C                     +  -3  -1   0   1   3   2
C                      +------------------------+
C               I   -3 I -3  -3  -3   2   2   2 I
C               S   -1 I -3  -1  -1   2   2   2 I
C               M    0 I -3  -1   0   1   3   2 I
C               O    1 I  2   2   1   1   3   2 I
C               N    3 I  2   2   3   3   3   2 I
C              (N)   2 I  2   2   2   2   2   2 I
C                      +------------------------+
C           Note that the 2 row and column are out of order so as not
C           to obscure the symmetry in the rest of the table.
C
C           No change needed if equal or constant on this interval or
C           already declared nonmonotonic.
            IF ( (ISMON(I).NE.ISMON(N)) .AND. (ISMON(I).NE.0)
     .                                  .AND. (ISMON(N).NE.2) )  THEN
               IF ( (ISMON(I).EQ.2) .OR. (ISMON(N).EQ.0) )  THEN
                  ISMON(N) =  ISMON(I)
               ELSE IF (ISMON(I)*ISMON(N) .LT. 0)  THEN
C                 This interval has opposite sense from curve so far.
                  ISMON(N) = 2
               ELSE
C                 At this point, both are nonzero with same sign, and
C                 we have already eliminated case both +-1.
                  ISMON(N) = ISIGN (3, ISMON(N))
               ENDIF
            ENDIF
         ENDIF
   90 CONTINUE
C
C  NORMAL RETURN.
C
      IERR = 0
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHCM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHCM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHCM',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      RETURN
C------------- LAST LINE OF DPCHCM FOLLOWS -----------------------------
      END
