      SUBROUTINE SPOCO (A, LDA, N, RCOND, Z, INFO)
      INTEGER LDA,N,INFO
      REAL A(LDA,*),Z(*)
      REAL RCOND
C
      REAL SDOT,EK,T,WK,WKM
      REAL ANORM,S,SASUM,SM,YNORM
      INTEGER I,J,JM1,K,KB,KP1
C
C     FIND NORM OF A USING ONLY UPPER HALF
C
C***FIRST EXECUTABLE STATEMENT  SPOCO
      DO 30 J = 1, N
         Z(J) = SASUM(J,A(1,J),1)
         JM1 = J - 1
         IF (JM1 .LT. 1) GO TO 20
         DO 10 I = 1, JM1
            Z(I) = Z(I) + ABS(A(I,J))
   10    CONTINUE
   20    CONTINUE
   30 CONTINUE
      ANORM = 0.0E0
      DO 40 J = 1, N
         ANORM = MAX(ANORM,Z(J))
   40 CONTINUE
C
C     FACTOR
C
      CALL SPOFA(A,LDA,N,INFO)
      IF (INFO .NE. 0) GO TO 180
C
C        RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C        ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  A*Y = E .
C        THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
C        GROWTH IN THE ELEMENTS OF W  WHERE  TRANS(R)*W = E .
C        THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
C
C        SOLVE TRANS(R)*W = E
C
         EK = 1.0E0
         DO 50 J = 1, N
            Z(J) = 0.0E0
   50    CONTINUE
         DO 110 K = 1, N
            IF (Z(K) .NE. 0.0E0) EK = SIGN(EK,-Z(K))
            IF (ABS(EK-Z(K)) .LE. A(K,K)) GO TO 60
               S = A(K,K)/ABS(EK-Z(K))
               CALL SSCAL(N,S,Z,1)
               EK = S*EK
   60       CONTINUE
            WK = EK - Z(K)
            WKM = -EK - Z(K)
            S = ABS(WK)
            SM = ABS(WKM)
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
            KP1 = K + 1
            IF (KP1 .GT. N) GO TO 100
               DO 70 J = KP1, N
                  SM = SM + ABS(Z(J)+WKM*A(K,J))
                  Z(J) = Z(J) + WK*A(K,J)
                  S = S + ABS(Z(J))
   70          CONTINUE
               IF (S .GE. SM) GO TO 90
                  T = WKM - WK
                  WK = WKM
                  DO 80 J = KP1, N
                     Z(J) = Z(J) + T*A(K,J)
   80             CONTINUE
   90          CONTINUE
  100       CONTINUE
            Z(K) = WK
  110    CONTINUE
         S = 1.0E0/SASUM(N,Z,1)
         CALL SSCAL(N,S,Z,1)
C
C        SOLVE R*Y = W
C
         DO 130 KB = 1, N
            K = N + 1 - KB
            IF (ABS(Z(K)) .LE. A(K,K)) GO TO 120
               S = A(K,K)/ABS(Z(K))
               CALL SSCAL(N,S,Z,1)
  120       CONTINUE
            Z(K) = Z(K)/A(K,K)
            T = -Z(K)
            CALL SAXPY(K-1,T,A(1,K),1,Z(1),1)
  130    CONTINUE
         S = 1.0E0/SASUM(N,Z,1)
         CALL SSCAL(N,S,Z,1)
C
         YNORM = 1.0E0
C
C        SOLVE TRANS(R)*V = Y
C
         DO 150 K = 1, N
            Z(K) = Z(K) - SDOT(K-1,A(1,K),1,Z(1),1)
            IF (ABS(Z(K)) .LE. A(K,K)) GO TO 140
               S = A(K,K)/ABS(Z(K))
               CALL SSCAL(N,S,Z,1)
               YNORM = S*YNORM
  140       CONTINUE
            Z(K) = Z(K)/A(K,K)
  150    CONTINUE
         S = 1.0E0/SASUM(N,Z,1)
         CALL SSCAL(N,S,Z,1)
         YNORM = S*YNORM
C
C        SOLVE R*Z = V
C
         DO 170 KB = 1, N
            K = N + 1 - KB
            IF (ABS(Z(K)) .LE. A(K,K)) GO TO 160
               S = A(K,K)/ABS(Z(K))
               CALL SSCAL(N,S,Z,1)
               YNORM = S*YNORM
  160       CONTINUE
            Z(K) = Z(K)/A(K,K)
            T = -Z(K)
            CALL SAXPY(K-1,T,A(1,K),1,Z(1),1)
  170    CONTINUE
C        MAKE ZNORM = 1.0
         S = 1.0E0/SASUM(N,Z,1)
         CALL SSCAL(N,S,Z,1)
         YNORM = S*YNORM
C
         IF (ANORM .NE. 0.0E0) RCOND = YNORM/ANORM
         IF (ANORM .EQ. 0.0E0) RCOND = 0.0E0
  180 CONTINUE
      RETURN
      END
