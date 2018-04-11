      FUNCTION PPGSF (X, IZ, C, A, BH)
      DIMENSION       A(*)       ,C(*)       ,BH(*)
C***FIRST EXECUTABLE STATEMENT  PPGSF
      SUM = 0.
      DO 101 J=1,IZ
         SUM = SUM-1./(X-BH(J))**2
  101 CONTINUE
      PPGSF = SUM
      RETURN
      END
