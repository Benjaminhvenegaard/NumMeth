      SUBROUTINE SPBSL (ABD, LDA, N, M, B)
      INTEGER LDA,N,M
      REAL ABD(LDA,*),B(*)
C
      REAL SDOT,T
      INTEGER K,KB,LA,LB,LM
C
C     SOLVE TRANS(R)*Y = B
C
C***FIRST EXECUTABLE STATEMENT  SPBSL
      DO 10 K = 1, N
         LM = MIN(K-1,M)
         LA = M + 1 - LM
         LB = K - LM
         T = SDOT(LM,ABD(LA,K),1,B(LB),1)
         B(K) = (B(K) - T)/ABD(M+1,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         LM = MIN(K-1,M)
         LA = M + 1 - LM
         LB = K - LM
         B(K) = B(K)/ABD(M+1,K)
         T = -B(K)
         CALL SAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   20 CONTINUE
      RETURN
      END
