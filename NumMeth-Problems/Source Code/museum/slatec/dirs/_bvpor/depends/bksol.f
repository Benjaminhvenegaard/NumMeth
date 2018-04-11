      SUBROUTINE BKSOL (N, A, X)
C
      DIMENSION A(*),X(*)
C
C***FIRST EXECUTABLE STATEMENT  BKSOL
      M=(N*(N+1))/2
      X(N)=X(N)*A(M)
      IF (N .EQ. 1) GO TO 20
      NM1=N-1
      DO 10 K=1,NM1
      J=N-K
      M=M-K-1
   10 X(J)=X(J)*A(M) - SDOT(K,A(M+1),1,X(J+1),1)
C
   20 RETURN
      END
