      SUBROUTINE DGBCO (ABD, LDA, N, ML, MU, IPVT, RCOND, Z)
      INTEGER LDA,N,ML,MU,IPVT(*)
      DOUBLE PRECISION ABD(LDA,*),Z(*)
      DOUBLE PRECISION RCOND
C
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER IS,INFO,J,JU,K,KB,KP1,L,LA,LM,LZ,M,MM
C
C     COMPUTE 1-NORM OF A
C
C***FIRST EXECUTABLE STATEMENT  DGBCO
      ANORM = 0.0D0
      L = ML + 1
      IS = L + MU
      DO 10 J = 1, N
         ANORM = MAX(ANORM,DASUM(L,ABD(IS,J),1))
         IF (IS .GT. ML + 1) IS = IS - 1
         IF (J .LE. MU) L = L + 1
         IF (J .GE. N - ML) L = L - 1
   10 CONTINUE
C
C     FACTOR
C
      CALL DGBFA(ABD,LDA,N,ML,MU,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = 1.0D0
      DO 20 J = 1, N
         Z(J) = 0.0D0
   20 CONTINUE
      M = ML + MU + 1
      JU = 0
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0D0) EK = SIGN(EK,-Z(K))
         IF (ABS(EK-Z(K)) .LE. ABS(ABD(M,K))) GO TO 30
            S = ABS(ABD(M,K))/ABS(EK-Z(K))
            CALL DSCAL(N,S,Z,1)
            EK = S*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = ABS(WK)
         SM = ABS(WKM)
         IF (ABD(M,K) .EQ. 0.0D0) GO TO 40
            WK = WK/ABD(M,K)
            WKM = WKM/ABD(M,K)
         GO TO 50
   40    CONTINUE
            WK = 1.0D0
            WKM = 1.0D0
   50    CONTINUE
         KP1 = K + 1
         JU = MIN(MAX(JU,MU+IPVT(K)),N)
         MM = M
         IF (KP1 .GT. JU) GO TO 90
            DO 60 J = KP1, JU
               MM = MM - 1
               SM = SM + ABS(Z(J)+WKM*ABD(MM,J))
               Z(J) = Z(J) + WK*ABD(MM,J)
               S = S + ABS(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               MM = M
               DO 70 J = KP1, JU
                  MM = MM - 1
                  Z(J) = Z(J) + T*ABD(MM,J)
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
C     SOLVE TRANS(L)*Y = W
C
      DO 120 KB = 1, N
         K = N + 1 - KB
         LM = MIN(ML,N-K)
         IF (K .LT. N) Z(K) = Z(K) + DDOT(LM,ABD(M+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0D0) GO TO 110
            S = 1.0D0/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
C
      YNORM = 1.0D0
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         LM = MIN(ML,N-K)
         IF (K .LT. N) CALL DAXPY(LM,T,ABD(M+1,K),1,Z(K+1),1)
         IF (ABS(Z(K)) .LE. 1.0D0) GO TO 130
            S = 1.0D0/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = W
C
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (ABS(Z(K)) .LE. ABS(ABD(M,K))) GO TO 150
            S = ABS(ABD(M,K))/ABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (ABD(M,K) .NE. 0.0D0) Z(K) = Z(K)/ABD(M,K)
         IF (ABD(M,K) .EQ. 0.0D0) Z(K) = 1.0D0
         LM = MIN(K,M) - 1
         LA = M - LM
         LZ = K - LM
         T = -Z(K)
         CALL DAXPY(LM,T,ABD(LA,K),1,Z(LZ),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END
