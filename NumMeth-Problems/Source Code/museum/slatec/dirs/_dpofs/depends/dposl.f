      SUBROUTINE DPOSL (A, LDA, N, B)
      INTEGER LDA,N
      DOUBLE PRECISION A(LDA,*),B(*)
C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB
C
C     SOLVE TRANS(R)*Y = B
C
C***FIRST EXECUTABLE STATEMENT  DPOSL
      DO 10 K = 1, N
         T = DDOT(K-1,A(1,K),1,B(1),1)
         B(K) = (B(K) - T)/A(K,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   20 CONTINUE
      RETURN
      END
