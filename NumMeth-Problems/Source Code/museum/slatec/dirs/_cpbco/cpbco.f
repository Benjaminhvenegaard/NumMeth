      SUBROUTINE CPBCO (ABD, LDA, N, M, RCOND, Z, INFO)
      INTEGER LDA,N,M,INFO
      COMPLEX ABD(LDA,*),Z(*)
      REAL RCOND
C
      COMPLEX CDOTC,EK,T,WK,WKM
      REAL ANORM,S,SCASUM,SM,YNORM
      INTEGER I,J,J2,K,KB,KP1,L,LA,LB,LM,MU
      COMPLEX ZDUM,ZDUM2,CSIGN1
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
      CSIGN1(ZDUM,ZDUM2) = CABS1(ZDUM)*(ZDUM2/CABS1(ZDUM2))
C
C     FIND NORM OF A
C
C***FIRST EXECUTABLE STATEMENT  CPBCO
      DO 30 J = 1, N
         L = MIN(J,M+1)
         MU = MAX(M+2-J,1)
         Z(J) = CMPLX(SCASUM(L,ABD(MU,J),1),0.0E0)
         K = J - L
         IF (M .LT. MU) GO TO 20
         DO 10 I = MU, M
            K = K + 1
            Z(K) = CMPLX(REAL(Z(K))+CABS1(ABD(I,J)),0.0E0)
   10    CONTINUE
   20    CONTINUE
   30 CONTINUE
      ANORM = 0.0E0
      DO 40 J = 1, N
         ANORM = MAX(ANORM,REAL(Z(J)))
   40 CONTINUE
C
C     FACTOR
C
      CALL CPBFA(ABD,LDA,N,M,INFO)
      IF (INFO .NE. 0) GO TO 180
C
C        RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C        ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  A*Y = E .
C        THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
C        GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(R)*W = E .
C        THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
C
C        SOLVE CTRANS(R)*W = E
C
         EK = (1.0E0,0.0E0)
         DO 50 J = 1, N
            Z(J) = (0.0E0,0.0E0)
   50    CONTINUE
         DO 110 K = 1, N
            IF (CABS1(Z(K)) .NE. 0.0E0) EK = CSIGN1(EK,-Z(K))
            IF (CABS1(EK-Z(K)) .LE. REAL(ABD(M+1,K))) GO TO 60
               S = REAL(ABD(M+1,K))/CABS1(EK-Z(K))
               CALL CSSCAL(N,S,Z,1)
               EK = CMPLX(S,0.0E0)*EK
   60       CONTINUE
            WK = EK - Z(K)
            WKM = -EK - Z(K)
            S = CABS1(WK)
            SM = CABS1(WKM)
            WK = WK/ABD(M+1,K)
            WKM = WKM/ABD(M+1,K)
            KP1 = K + 1
            J2 = MIN(K+M,N)
            I = M + 1
            IF (KP1 .GT. J2) GO TO 100
               DO 70 J = KP1, J2
                  I = I - 1
                  SM = SM + CABS1(Z(J)+WKM*CONJG(ABD(I,J)))
                  Z(J) = Z(J) + WK*CONJG(ABD(I,J))
                  S = S + CABS1(Z(J))
   70          CONTINUE
               IF (S .GE. SM) GO TO 90
                  T = WKM - WK
                  WK = WKM
                  I = M + 1
                  DO 80 J = KP1, J2
                     I = I - 1
                     Z(J) = Z(J) + T*CONJG(ABD(I,J))
   80             CONTINUE
   90          CONTINUE
  100       CONTINUE
            Z(K) = WK
  110    CONTINUE
         S = 1.0E0/SCASUM(N,Z,1)
         CALL CSSCAL(N,S,Z,1)
C
C        SOLVE  R*Y = W
C
         DO 130 KB = 1, N
            K = N + 1 - KB
            IF (CABS1(Z(K)) .LE. REAL(ABD(M+1,K))) GO TO 120
               S = REAL(ABD(M+1,K))/CABS1(Z(K))
               CALL CSSCAL(N,S,Z,1)
  120       CONTINUE
            Z(K) = Z(K)/ABD(M+1,K)
            LM = MIN(K-1,M)
            LA = M + 1 - LM
            LB = K - LM
            T = -Z(K)
            CALL CAXPY(LM,T,ABD(LA,K),1,Z(LB),1)
  130    CONTINUE
         S = 1.0E0/SCASUM(N,Z,1)
         CALL CSSCAL(N,S,Z,1)
C
         YNORM = 1.0E0
C
C        SOLVE CTRANS(R)*V = Y
C
         DO 150 K = 1, N
            LM = MIN(K-1,M)
            LA = M + 1 - LM
            LB = K - LM
            Z(K) = Z(K) - CDOTC(LM,ABD(LA,K),1,Z(LB),1)
            IF (CABS1(Z(K)) .LE. REAL(ABD(M+1,K))) GO TO 140
               S = REAL(ABD(M+1,K))/CABS1(Z(K))
               CALL CSSCAL(N,S,Z,1)
               YNORM = S*YNORM
  140       CONTINUE
            Z(K) = Z(K)/ABD(M+1,K)
  150    CONTINUE
         S = 1.0E0/SCASUM(N,Z,1)
         CALL CSSCAL(N,S,Z,1)
         YNORM = S*YNORM
C
C        SOLVE  R*Z = W
C
         DO 170 KB = 1, N
            K = N + 1 - KB
            IF (CABS1(Z(K)) .LE. REAL(ABD(M+1,K))) GO TO 160
               S = REAL(ABD(M+1,K))/CABS1(Z(K))
               CALL CSSCAL(N,S,Z,1)
               YNORM = S*YNORM
  160       CONTINUE
            Z(K) = Z(K)/ABD(M+1,K)
            LM = MIN(K-1,M)
            LA = M + 1 - LM
            LB = K - LM
            T = -Z(K)
            CALL CAXPY(LM,T,ABD(LA,K),1,Z(LB),1)
  170    CONTINUE
C        MAKE ZNORM = 1.0
         S = 1.0E0/SCASUM(N,Z,1)
         CALL CSSCAL(N,S,Z,1)
         YNORM = S*YNORM
C
         IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
         IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
  180 CONTINUE
      RETURN
      END
