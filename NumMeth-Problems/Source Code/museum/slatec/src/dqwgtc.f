      DOUBLE PRECISION FUNCTION DQWGTC (X, C, P2, P3, P4, KP)
C
      DOUBLE PRECISION C,P2,P3,P4,X
      INTEGER KP
C***FIRST EXECUTABLE STATEMENT  DQWGTC
      DQWGTC = 0.1D+01/(X-C)
      RETURN
      END