      SUBROUTINE SPOSL (A, LDA, N, B)
      INTEGER LDA,N
      REAL A(LDA,*),B(*)
C
      REAL SDOT,T
      INTEGER K,KB
C
C     SOLVE TRANS(R)*Y = B
C
C***FIRST EXECUTABLE STATEMENT  SPOSL
      DO 10 K = 1, N
         T = SDOT(K-1,A(1,K),1,B(1),1)
         B(K) = (B(K) - T)/A(K,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL SAXPY(K-1,T,A(1,K),1,B(1),1)
   20 CONTINUE
      RETURN
      END
