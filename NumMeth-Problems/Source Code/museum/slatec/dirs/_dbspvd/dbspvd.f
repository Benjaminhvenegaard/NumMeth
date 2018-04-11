      SUBROUTINE DBSPVD (T, K, NDERIV, X, ILEFT, LDVNIK, VNIKX, WORK)
C
      INTEGER I,IDERIV,ILEFT,IPKMD,J,JJ,JLOW,JM,JP1MID,K,KMD, KP1, L,
     1 LDUMMY, M, MHIGH, NDERIV
      DOUBLE PRECISION FACTOR, FKMD, T, V, VNIKX, WORK, X
C     DIMENSION T(ILEFT+K), WORK((K+1)*(K+2)/2)
C     A(I,J) = WORK(I+J*(J+1)/2),  I=1,J+1  J=1,K-1
C     A(I,K) = W0RK(I+K*(K-1)/2)  I=1.K
C     WORK(1) AND WORK((K+1)*(K+2)/2) ARE NOT USED.
      DIMENSION T(*), VNIKX(LDVNIK,*), WORK(*)
C***FIRST EXECUTABLE STATEMENT  DBSPVD
      IF(K.LT.1) GO TO 200
      IF(NDERIV.LT.1 .OR. NDERIV.GT.K) GO TO 205
      IF(LDVNIK.LT.K) GO TO 210
      IDERIV = NDERIV
      KP1 = K + 1
      JJ = KP1 - IDERIV
      CALL DBSPVN(T, JJ, K, 1, X, ILEFT, VNIKX, WORK, IWORK)
      IF (IDERIV.EQ.1) GO TO 100
      MHIGH = IDERIV
      DO 20 M=2,MHIGH
        JP1MID = 1
        DO 10 J=IDERIV,K
          VNIKX(J,IDERIV) = VNIKX(JP1MID,1)
          JP1MID = JP1MID + 1
   10   CONTINUE
        IDERIV = IDERIV - 1
        JJ = KP1 - IDERIV
        CALL DBSPVN(T, JJ, K, 2, X, ILEFT, VNIKX, WORK, IWORK)
   20 CONTINUE
C
      JM = KP1*(KP1+1)/2
      DO 30 L = 1,JM
        WORK(L) = 0.0D0
   30 CONTINUE
C     A(I,I) = WORK(I*(I+3)/2) = 1.0       I = 1,K
      L = 2
      J = 0
      DO 40 I = 1,K
        J = J + L
        WORK(J) = 1.0D0
        L = L + 1
   40 CONTINUE
      KMD = K
      DO 90 M=2,MHIGH
        KMD = KMD - 1
        FKMD = KMD
        I = ILEFT
        J = K
        JJ = J*(J+1)/2
        JM = JJ - J
        DO 60 LDUMMY=1,KMD
          IPKMD = I + KMD
          FACTOR = FKMD/(T(IPKMD)-T(I))
          DO 50 L=1,J
            WORK(L+JJ) = (WORK(L+JJ)-WORK(L+JM))*FACTOR
   50     CONTINUE
          I = I - 1
          J = J - 1
          JJ = JM
          JM = JM - J
   60   CONTINUE
C
        DO 80 I=1,K
          V = 0.0D0
          JLOW = MAX(I,M)
          JJ = JLOW*(JLOW+1)/2
          DO 70 J=JLOW,K
            V = WORK(I+JJ)*VNIKX(J,M) + V
            JJ = JJ + J + 1
   70     CONTINUE
          VNIKX(I,M) = V
   80   CONTINUE
   90 CONTINUE
  100 RETURN
C
C
  200 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPVD', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  205 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPVD',
     +   'NDERIV DOES NOT SATISFY 1.LE.NDERIV.LE.K', 2, 1)
      RETURN
  210 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPVD',
     +   'LDVNIK DOES NOT SATISFY LDVNIK.GE.K', 2, 1)
      RETURN
      END
