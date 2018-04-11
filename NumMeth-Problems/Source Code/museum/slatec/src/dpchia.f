      DOUBLE PRECISION FUNCTION DPCHIA (N, X, F, D, INCFD, SKIP, A, B,
     +   IERR)
C
C  Programming notes:
C  1. The error flag from DPCHID is tested, because a logic flaw
C     could conceivably result in IERD=-4, which should be reported.
C**End
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), A, B
      LOGICAL  SKIP
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, IA, IB, IERD, IL, IR
      DOUBLE PRECISION  VALUE, XA, XB, ZERO
      SAVE ZERO
      DOUBLE PRECISION  DCHFIE, DPCHID
C
C  INITIALIZE.
C
      DATA  ZERO /0.D0/
C***FIRST EXECUTABLE STATEMENT  DPCHIA
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
      IERR = 0
      IF ( (A.LT.X(1)) .OR. (A.GT.X(N)) )  IERR = IERR + 1
      IF ( (B.LT.X(1)) .OR. (B.GT.X(N)) )  IERR = IERR + 2
C
C  COMPUTE INTEGRAL VALUE.
C
      IF (A .NE. B)  THEN
         XA = MIN (A, B)
         XB = MAX (A, B)
         IF (XB .LE. X(2))  THEN
C           INTERVAL IS TO LEFT OF X(2), SO USE FIRST CUBIC.
C                   ---------------------------------------
            VALUE = DCHFIE (X(1),X(2), F(1,1),F(1,2),
     +                                 D(1,1),D(1,2), A, B)
C                   ---------------------------------------
         ELSE IF (XA .GE. X(N-1))  THEN
C           INTERVAL IS TO RIGHT OF X(N-1), SO USE LAST CUBIC.
C                   ------------------------------------------
            VALUE = DCHFIE(X(N-1),X(N), F(1,N-1),F(1,N),
     +                                  D(1,N-1),D(1,N), A, B)
C                   ------------------------------------------
         ELSE
C           'NORMAL' CASE -- XA.LT.XB, XA.LT.X(N-1), XB.GT.X(2).
C      ......LOCATE IA AND IB SUCH THAT
C               X(IA-1).LT.XA.LE.X(IA).LE.X(IB).LE.XB.LE.X(IB+1)
            IA = 1
            DO 10  I = 1, N-1
               IF (XA .GT. X(I))  IA = I + 1
   10       CONTINUE
C             IA = 1 IMPLIES XA.LT.X(1) .  OTHERWISE,
C             IA IS LARGEST INDEX SUCH THAT X(IA-1).LT.XA,.
C
            IB = N
            DO 20  I = N, IA, -1
               IF (XB .LT. X(I))  IB = I - 1
   20       CONTINUE
C             IB = N IMPLIES XB.GT.X(N) .  OTHERWISE,
C             IB IS SMALLEST INDEX SUCH THAT XB.LT.X(IB+1) .
C
C     ......COMPUTE THE INTEGRAL.
            IF (IB .LT. IA)  THEN
C              THIS MEANS IB = IA-1 AND
C                 (A,B) IS A SUBSET OF (X(IB),X(IA)).
C                      -------------------------------------------
               VALUE = DCHFIE (X(IB),X(IA), F(1,IB),F(1,IA),
     +                                      D(1,IB),D(1,IA), A, B)
C                      -------------------------------------------
            ELSE
C
C              FIRST COMPUTE INTEGRAL OVER (X(IA),X(IB)).
C                (Case (IB .EQ. IA) is taken care of by initialization
C                 of VALUE to ZERO.)
               IF (IB .GT. IA)  THEN
C                         ---------------------------------------------
                  VALUE = DPCHID (N, X, F, D, INCFD, SKIP, IA, IB, IERD)
C                         ---------------------------------------------
                  IF (IERD .LT. 0)  GO TO 5004
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (XA,X(IA)).
               IF (XA .LT. X(IA))  THEN
                  IL = MAX(1, IA-1)
                  IR = IL + 1
C                                 -------------------------------------
                  VALUE = VALUE + DCHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), XA, X(IA))
C                                 -------------------------------------
               ENDIF
C
C              THEN ADD ON INTEGRAL OVER (X(IB),XB).
               IF (XB .GT. X(IB))  THEN
                  IR = MIN (IB+1, N)
                  IL = IR - 1
C                                 -------------------------------------
                  VALUE = VALUE + DCHFIE (X(IL),X(IR), F(1,IL),F(1,IR),
     +                                      D(1,IL),D(1,IR), X(IB), XB)
C                                 -------------------------------------
               ENDIF
C
C              FINALLY, ADJUST SIGN IF NECESSARY.
               IF (A .GT. B)  VALUE = -VALUE
            ENDIF
         ENDIF
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      DPCHIA = VALUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'DPCHIA',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      GO TO 5000
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'DPCHIA', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      GO TO 5000
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'DPCHIA',
     +   'X-ARRAY NOT STRICTLY INCREASING', IERR, 1)
      GO TO 5000
C
 5004 CONTINUE
C     TROUBLE IN DPCHID.  (SHOULD NEVER OCCUR.)
      IERR = -4
      CALL XERMSG ('SLATEC', 'DPCHIA', 'TROUBLE IN DPCHID', IERR, 1)
      GO TO 5000
C------------- LAST LINE OF DPCHIA FOLLOWS -----------------------------
      END
