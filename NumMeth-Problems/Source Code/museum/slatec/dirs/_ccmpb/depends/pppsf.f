      FUNCTION PPPSF (X, IZ, C, A, BH)
      DIMENSION       A(*)       ,C(*)       ,BH(*)
C***FIRST EXECUTABLE STATEMENT  PPPSF
      SUM = 0.
      DO 101 J=1,IZ
         SUM = SUM+1./(X-BH(J))
  101 CONTINUE
      PPPSF = SUM
      RETURN
      END
