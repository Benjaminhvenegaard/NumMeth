      FUNCTION PPSGF (X, IZ, C, A, BH)
      DIMENSION       A(*)       ,C(*)       ,BH(*)
C***FIRST EXECUTABLE STATEMENT  PPSGF
      SUM = 0.
      DO 101 J=1,IZ
         SUM = SUM-1./(X-BH(J))**2
  101 CONTINUE
      PPSGF = SUM
      RETURN
      END
