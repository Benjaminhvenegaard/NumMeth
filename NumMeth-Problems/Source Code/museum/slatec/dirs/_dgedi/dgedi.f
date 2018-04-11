      SUBROUTINE DGEDI (A, LDA, N, IPVT, DET, WORK, JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),DET(2),WORK(*)
C
      DOUBLE PRECISION T
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
C***FIRST EXECUTABLE STATEMENT  DGEDI
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0D0
         DET(2) = 0.0D0
         TEN = 10.0D0
         DO 50 I = 1, N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
            IF (DET(1) .EQ. 0.0D0) GO TO 60
   10       IF (ABS(DET(1)) .GE. 1.0D0) GO TO 20
               DET(1) = TEN*DET(1)
               DET(2) = DET(2) - 1.0D0
            GO TO 10
   20       CONTINUE
   30       IF (ABS(DET(1)) .LT. TEN) GO TO 40
               DET(1) = DET(1)/TEN
               DET(2) = DET(2) + 1.0D0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(U)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 150
         DO 100 K = 1, N
            A(K,K) = 1.0D0/A(K,K)
            T = -A(K,K)
            CALL DSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0D0
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM INVERSE(U)*INVERSE(L)
C
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 140
         DO 130 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = 0.0D0
  110       CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL DAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END
