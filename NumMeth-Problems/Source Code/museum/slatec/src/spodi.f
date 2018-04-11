      SUBROUTINE SPODI (A, LDA, N, DET, JOB)
      INTEGER LDA,N,JOB
      REAL A(LDA,*)
      REAL DET(2)
C
      REAL T
      REAL S
      INTEGER I,J,JM1,K,KP1
C***FIRST EXECUTABLE STATEMENT  SPODI
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         S = 10.0E0
         DO 50 I = 1, N
            DET(1) = A(I,I)**2*DET(1)
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
         DO 100 K = 1, N
            A(K,K) = 1.0E0/A(K,K)
            T = -A(K,K)
            CALL SSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0E0
               CALL SAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * TRANS(INVERSE(R))
C
         DO 130 J = 1, N
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = A(K,J)
               CALL SAXPY(K,T,A(1,J),1,A(1,K),1)
  110       CONTINUE
  120       CONTINUE
            T = A(J,J)
            CALL SSCAL(J,T,A(1,J),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
