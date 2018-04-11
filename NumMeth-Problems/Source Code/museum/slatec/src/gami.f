      FUNCTION GAMI (A, X)
C***FIRST EXECUTABLE STATEMENT  GAMI
      IF (A .LE. 0.0) CALL XERMSG ('SLATEC', 'GAMI',
     +   'A MUST BE GT ZERO', 1, 2)
      IF (X .LT. 0.0) CALL XERMSG ('SLATEC', 'GAMI',
     +   'X MUST BE GE ZERO', 2, 2)
C
      GAMI = 0.0
      IF (X.EQ.0.0) RETURN
C
C THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
      FACTOR = EXP (ALNGAM(A) + A*LOG(X) )
C
      GAMI = FACTOR * GAMIT(A, X)
C
      RETURN
      END