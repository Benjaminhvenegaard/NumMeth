      SUBROUTINE DBKSOL (N, A, X)
C
      DOUBLE PRECISION DDOT
      INTEGER J, K, M, N, NM1
      DOUBLE PRECISION A(*), X(*)
C
C***FIRST EXECUTABLE STATEMENT  DBKSOL
      M = (N*(N + 1))/2
      X(N) = X(N)*A(M)
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 20
      DO 10 K = 1, NM1
         J = N - K
         M = M - K - 1
         X(J) = X(J)*A(M) - DDOT(K,A(M+1),1,X(J+1),1)
   10 CONTINUE
   20 CONTINUE
C
      RETURN
      END
