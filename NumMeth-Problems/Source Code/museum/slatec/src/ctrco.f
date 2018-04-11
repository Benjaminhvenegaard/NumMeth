      SUBROUTINE CTRCO (T, LDT, N, RCOND, Z, JOB)
      INTEGER LDT,N,JOB
      COMPLEX T(LDT,*),Z(*)
      REAL RCOND
C
      COMPLEX W,WK,WKM,EK
      REAL TNORM,YNORM,S,SM,SCASUM
      INTEGER I1,J,J1,J2,K,KK,L
      LOGICAL LOWER
      COMPLEX ZDUM,ZDUM1,ZDUM2,CSIGN1
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
      CSIGN1(ZDUM1,ZDUM2) = CABS1(ZDUM1)*(ZDUM2/CABS1(ZDUM2))
C
C***FIRST EXECUTABLE STATEMENT  CTRCO
      LOWER = JOB .EQ. 0
C
C     COMPUTE 1-NORM OF T
C
      TNORM = 0.0E0
      DO 10 J = 1, N
         L = J
         IF (LOWER) L = N + 1 - J
         I1 = 1
         IF (LOWER) I1 = J
         TNORM = MAX(TNORM,SCASUM(L,T(I1,J),1))
   10 CONTINUE
C
C     RCOND = 1/(NORM(T)*(ESTIMATE OF NORM(INVERSE(T)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  T*Z = Y  AND  CTRANS(T)*Y = E .
C     CTRANS(T)  IS THE CONJUGATE TRANSPOSE OF T .
C     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
C     GROWTH IN THE ELEMENTS OF Y .
C     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
C
C     SOLVE CTRANS(T)*Y = E
C
      EK = (1.0E0,0.0E0)
      DO 20 J = 1, N
         Z(J) = (0.0E0,0.0E0)
   20 CONTINUE
      DO 100 KK = 1, N
         K = KK
         IF (LOWER) K = N + 1 - KK
         IF (CABS1(Z(K)) .NE. 0.0E0) EK = CSIGN1(EK,-Z(K))
         IF (CABS1(EK-Z(K)) .LE. CABS1(T(K,K))) GO TO 30
            S = CABS1(T(K,K))/CABS1(EK-Z(K))
            CALL CSSCAL(N,S,Z,1)
            EK = CMPLX(S,0.0E0)*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = CABS1(WK)
         SM = CABS1(WKM)
         IF (CABS1(T(K,K)) .EQ. 0.0E0) GO TO 40
            WK = WK/CONJG(T(K,K))
            WKM = WKM/CONJG(T(K,K))
         GO TO 50
   40    CONTINUE
            WK = (1.0E0,0.0E0)
            WKM = (1.0E0,0.0E0)
   50    CONTINUE
         IF (KK .EQ. N) GO TO 90
            J1 = K + 1
            IF (LOWER) J1 = 1
            J2 = N
            IF (LOWER) J2 = K - 1
            DO 60 J = J1, J2
               SM = SM + CABS1(Z(J)+WKM*CONJG(T(K,J)))
               Z(J) = Z(J) + WK*CONJG(T(K,J))
               S = S + CABS1(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               W = WKM - WK
               WK = WKM
               DO 70 J = J1, J2
                  Z(J) = Z(J) + W*CONJG(T(K,J))
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
C
      YNORM = 1.0E0
C
C     SOLVE T*Z = Y
C
      DO 130 KK = 1, N
         K = N + 1 - KK
         IF (LOWER) K = KK
         IF (CABS1(Z(K)) .LE. CABS1(T(K,K))) GO TO 110
            S = CABS1(T(K,K))/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
            YNORM = S*YNORM
  110    CONTINUE
         IF (CABS1(T(K,K)) .NE. 0.0E0) Z(K) = Z(K)/T(K,K)
         IF (CABS1(T(K,K)) .EQ. 0.0E0) Z(K) = (1.0E0,0.0E0)
         I1 = 1
         IF (LOWER) I1 = K + 1
         IF (KK .GE. N) GO TO 120
            W = -Z(K)
            CALL CAXPY(N-KK,W,T(I1,K),1,Z(I1),1)
  120    CONTINUE
  130 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (TNORM .NE. 0.0E0) RCOND = YNORM/TNORM
      IF (TNORM .EQ. 0.0E0) RCOND = 0.0E0
      RETURN
      END