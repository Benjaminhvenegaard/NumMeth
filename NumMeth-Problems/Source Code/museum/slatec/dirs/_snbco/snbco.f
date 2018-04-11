      SUBROUTINE SNBCO (ABE, LDA, N, ML, MU, IPVT, RCOND, Z)
      INTEGER LDA,N,ML,MU,IPVT(*)
      REAL ABE(LDA,*),Z(*)
      REAL RCOND
C
      REAL SDOT,EK,T,WK,WKM
      REAL ANORM,S,SASUM,SM,YNORM
      INTEGER I,INFO,J,JU,K,KB,KP1,L,LDB,LM,LZ,M,ML1,MM,NL,NU
C***FIRST EXECUTABLE STATEMENT  SNBCO
      ML1=ML+1
      LDB = LDA - 1
      ANORM = 0.0E0
      DO 10 J = 1, N
        NU = MIN(MU,J-1)
        NL = MIN(ML,N-J)
        L = 1 + NU + NL
        ANORM = MAX(ANORM,SASUM(L,ABE(J+NL,ML1-NL),LDB))
   10 CONTINUE
C
C     FACTOR
C
      CALL SNBFA(ABE,LDA,N,ML,MU,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF  W WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = 1.0E0
      DO 20 J = 1, N
        Z(J) = 0.0E0
   20 CONTINUE
      M = ML + MU + 1
      JU = 0
      DO 100 K = 1, N
        IF (Z(K) .NE. 0.0E0) EK = SIGN(EK,-Z(K))
        IF (ABS(EK-Z(K)) .LE. ABS(ABE(K,ML1))) GO TO 30
          S = ABS(ABE(K,ML1))/ABS(EK-Z(K))
          CALL SSCAL(N,S,Z,1)
          EK = S*EK
   30   CONTINUE
        WK = EK - Z(K)
        WKM = -EK - Z(K)
        S = ABS(WK)
        SM = ABS(WKM)
        IF (ABE(K,ML1) .EQ. 0.0E0) GO TO 40
          WK = WK/ABE(K,ML1)
          WKM = WKM/ABE(K,ML1)
        GO TO 50
   40   CONTINUE
          WK = 1.0E0
          WKM = 1.0E0
   50   CONTINUE
        KP1 = K + 1
        JU = MIN(MAX(JU,MU+IPVT(K)),N)
        MM = ML1
        IF (KP1 .GT. JU) GO TO 90
          DO 60 I = KP1, JU
            MM = MM + 1
            SM = SM + ABS(Z(I)+WKM*ABE(K,MM))
            Z(I) = Z(I) + WK*ABE(K,MM)
            S = S + ABS(Z(I))
   60     CONTINUE
          IF (S .GE. SM) GO TO 80
            T = WKM -WK
            WK = WKM
            MM = ML1
            DO 70 I = KP1, JU
              MM = MM + 1
              Z(I) = Z(I) + T*ABE(K,MM)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE
      Z(K) = WK
  100 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
C
C     SOLVE TRANS(L)*Y = W
C
      DO 120 KB = 1, N
        K = N + 1 - KB
        NL = MIN(ML,N-K)
        IF (K .LT. N) Z(K) = Z(K) + SDOT(NL,ABE(K+NL,ML1-NL),-LDB,Z(K+1)
     1  ,1)
        IF (ABS(Z(K)) .LE. 1.0E0) GO TO 110
          S = 1.0E0/ABS(Z(K))
          CALL SSCAL(N,S,Z,1)
  110   CONTINUE
        L = IPVT(K)
        T = Z(L)
        Z(L) = Z(K)
        Z(K) = T
  120 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
C
      YNORM = 1.0E0
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
        L = IPVT(K)
        T = Z(L)
        Z(L) = Z(K)
        Z(K) = T
        NL = MIN(ML,N-K)
        IF (K .LT. N) CALL SAXPY(NL,T,ABE(K+NL,ML1-NL),-LDB,Z(K+1),1)
        IF (ABS(Z(K)) .LE. 1.0E0) GO TO 130
          S = 1.0E0/ABS(Z(K))
          CALL SSCAL(N,S,Z,1)
          YNORM = S*YNORM
  130   CONTINUE
  140 CONTINUE
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
      DO 160 KB = 1, N
        K = N + 1 - KB
        IF (ABS(Z(K)) .LE. ABS(ABE(K,ML1))) GO TO 150
          S = ABS(ABE(K,ML1))/ABS(Z(K))
          CALL SSCAL(N,S,Z,1)
          YNORM = S*YNORM
  150   CONTINUE
        IF (ABE(K,ML1) .NE. 0.0E0) Z(K) = Z(K)/ABE(K,ML1)
        IF (ABE(K,ML1) .EQ. 0.0E0) Z(K) = 1.0E0
        LM = MIN(K,M) - 1
        LZ = K - LM
        T = -Z(K)
        CALL SAXPY(LM,T,ABE(K-1,ML+2),-LDB,Z(LZ),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0E0
      S = 1.0E0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
      RETURN
      END
