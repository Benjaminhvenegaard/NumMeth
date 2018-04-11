      SUBROUTINE DBDIFF (L, V)
C
      INTEGER I, J, K, L
      DOUBLE PRECISION V
      DIMENSION V(*)
C***FIRST EXECUTABLE STATEMENT  DBDIFF
      IF (L.EQ.1) RETURN
      DO 20 J=2,L
        K = L
        DO 10 I=J,L
          V(K) = V(K-1) - V(K)
          K = K - 1
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
