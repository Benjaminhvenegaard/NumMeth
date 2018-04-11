      FUNCTION RUNIF (T, N)
      DIMENSION T(*)
      EXTERNAL RAND
      SAVE NOLD, FLOATN
      DATA NOLD /-1/
C***FIRST EXECUTABLE STATEMENT  RUNIF
      IF (N.EQ.NOLD) GO TO 20
C
      NOLD = ABS(N)
      FLOATN = NOLD
      IF (N.LT.0) DUMMY = RAND (T(NOLD+1))
      IF (N.LT.0) GO TO 20
C
      DO 10 I=1,NOLD
        T(I) = RAND (0.)
 10   CONTINUE
      T(NOLD+1) = RAND (0.)
C
 20   J = T(NOLD+1)*FLOATN + 1.
      T(NOLD+1) = T(J)
      RUNIF = T(J)
      T(J) = RAND (0.)
C
      RETURN
      END
