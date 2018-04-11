      SUBROUTINE CPPDI (AP, N, DET, JOB)
      INTEGER N,JOB
      COMPLEX AP(*)
      REAL DET(2)
C
      COMPLEX T
      REAL S
      INTEGER I,II,J,JJ,JM1,J1,K,KJ,KK,KP1,K1
C***FIRST EXECUTABLE STATEMENT  CPPDI
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         S = 10.0E0
         II = 0
         DO 50 I = 1, N
            II = II + I
            DET(1) = REAL(AP(II))**2*DET(1)
            IF (DET(1) .EQ. 0.0E0) GO TO 60
   10       IF (DET(1) .GE. 1.0E0) GO TO 20
               DET(1) = S*DET(1)
               DET(2) = DET(2) - 1.0E0
            GO TO 10
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40
               DET(1) = DET(1)/S
               DET(2) = DET(2) + 1.0E0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(R)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 140
         KK = 0
         DO 100 K = 1, N
            K1 = KK + 1
            KK = KK + K
            AP(KK) = (1.0E0,0.0E0)/AP(KK)
            T = -AP(KK)
            CALL CSCAL(K-1,T,AP(K1),1)
            KP1 = K + 1
            J1 = KK + 1
            KJ = KK + K
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = AP(KJ)
               AP(KJ) = (0.0E0,0.0E0)
               CALL CAXPY(K,T,AP(K1),1,AP(J1),1)
               J1 = J1 + J
               KJ = KJ + J
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * CTRANS(INVERSE(R))
C
         JJ = 0
         DO 130 J = 1, N
            J1 = JJ + 1
            JJ = JJ + J
            JM1 = J - 1
            K1 = 1
            KJ = J1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = CONJG(AP(KJ))
               CALL CAXPY(K,T,AP(J1),1,AP(K1),1)
               K1 = K1 + K
               KJ = KJ + 1
  110       CONTINUE
  120       CONTINUE
            T = CONJG(AP(JJ))
            CALL CSCAL(J,T,AP(J1),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
