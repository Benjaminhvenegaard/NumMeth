      DOUBLE PRECISION FUNCTION DPCHID (N, X, F, D, INCFD, SKIP, IA, IB,
     +   IERR)
C
C  Programming notes:
C  1. This routine uses a special formula that is valid only for
C     integrals whose limits coincide with data values.  This is
C     mathematically equivalent to, but much more efficient than,
C     calls to DCHFIE.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IA, IB, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*)
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IUP, LOW
      DOUBLE PRECISION  H, HALF, SIX, SUM, VALUE, ZERO
      SAVE ZERO, HALF, SIX
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/,  HALF/.5D0/, SIX/6.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHID
      VALUE = ZERO
C
C  VALIDITY-CHECK ARGUMENTS.
C
      IF (SKIP)  GO TO 5
C
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
    5 CONTINUE
      SKIP = .TRUE.
      IF ((IA.LT.1) .OR. (IA.GT.N))  GO TO 5004
      IF ((IB.LT.1) .OR. (IB.GT.N))  GO TO 5004
      IERR = 0
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (IA .NE. IB)  THEN
         LOW = MIN(IA, IB)
         IUP = MAX(IA, IB) - 1
         SUM = ZERO
         DO 10  I = LOW, IUP
            H = X(I+1) - X(I)
            SUM = SUM + H*( (F(1,I) + F(1,I+1)) +
     *                      (D(1,I) - D(1,I+1))*(H/SIX) )
   10    CONTINUE
         VALUE = HALF * SUM
         IF (IA .GT. IB)  VALUE = -VALUE
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      DPCHID = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHID',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHID', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHID',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     IA OR IB OUT OF RANGE RETURN.
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHID', 'IA OR IB OUT OF RANGE', IERR,
     +   1)
      GO TO 5000
C------------- LAST LINE OF DPCHID FOLLOWS -----------------------------
      END
