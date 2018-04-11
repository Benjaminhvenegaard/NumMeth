      SUBROUTINE SNBSL (ABE, LDA, N, ML, MU, IPVT, B, JOB)
      INTEGER LDA,N,ML,MU,IPVT(*),JOB
      REAL ABE(LDA,*),B(*)
C
      REAL SDOT,T
      INTEGER K,KB,L,LB,LDB,LM,M,MLM,NM1
C***FIRST EXECUTABLE STATEMENT  SNBSL
      M=MU+ML+1
      NM1=N-1
      LDB=1-LDA
      IF(JOB.NE.0)GO TO 50
C
C       JOB = 0 , SOLVE  A * X = B
C       FIRST SOLVE L*Y = B
C
        IF(ML.EQ.0)GO TO 30
        IF(NM1.LT.1)GO TO 30
          DO 20 K=1,NM1
            LM=MIN(ML,N-K)
            L=IPVT(K)
            T=B(L)
            IF(L.EQ.K)GO TO 10
              B(L)=B(K)
              B(K)=T
   10       CONTINUE
            MLM=ML-(LM-1)
            CALL SAXPY(LM,T,ABE(K+LM,MLM),LDB,B(K+1),1)
   20     CONTINUE
   30   CONTINUE
C
C       NOW SOLVE  U*X = Y
C
        DO 40 KB=1,N
          K=N+1-KB
          B(K)=B(K)/ABE(K,ML+1)
          LM=MIN(K,M)-1
          LB=K-LM
          T=-B(K)
          CALL SAXPY(LM,T,ABE(K-1,ML+2),LDB,B(LB),1)
   40   CONTINUE
      GO TO 100
   50 CONTINUE
C
C       JOB = NONZERO, SOLVE TRANS(A) * X = B
C       FIRST SOLVE  TRANS(U)*Y = B
C
        DO 60 K = 1, N
          LM = MIN(K,M) - 1
          LB = K - LM
          T = SDOT(LM,ABE(K-1,ML+2),LDB,B(LB),1)
          B(K) = (B(K) - T)/ABE(K,ML+1)
   60   CONTINUE
C
C       NOW SOLVE TRANS(L)*X = Y
C
        IF (ML .EQ. 0) GO TO 90
        IF (NM1 .LT. 1) GO TO 90
          DO 80 KB = 1, NM1
            K = N - KB
            LM = MIN(ML,N-K)
            MLM = ML - (LM - 1)
            B(K) = B(K) + SDOT(LM,ABE(K+LM,MLM),LDB,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
              T = B(L)
              B(L) = B(K)
              B(K) = T
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE
  100 CONTINUE
      RETURN
      END
