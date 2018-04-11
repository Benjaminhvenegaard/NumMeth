      SUBROUTINE MPMLP (U, V, W, J)
      INTEGER U(*), V(*), W
C***FIRST EXECUTABLE STATEMENT  MPMLP
      DO 10 I = 1, J
   10 U(I) = U(I) + W*V(I)
      RETURN
      END
