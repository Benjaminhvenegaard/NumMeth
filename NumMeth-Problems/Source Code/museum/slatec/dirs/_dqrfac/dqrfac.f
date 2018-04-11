      SUBROUTINE DQRFAC (M, N, A, LDA, PIVOT, IPVT, LIPVT, SIGMA,
     +   ACNORM, WA)
      INTEGER M,N,LDA,LIPVT
      INTEGER IPVT(*)
      LOGICAL PIVOT
      SAVE ONE, P05, ZERO
      DOUBLE PRECISION A(LDA,*),SIGMA(*),ACNORM(*),WA(*)
      INTEGER I,J,JP1,K,KMAX,MINMN
      DOUBLE PRECISION AJNORM,EPSMCH,ONE,P05,SUM,TEMP,ZERO
      DOUBLE PRECISION D1MACH,DENORM
      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DQRFAC
      EPSMCH = D1MACH(4)
C
C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.
C
      DO 10 J = 1, N
         ACNORM(J) = DENORM(M,A(1,J))
         SIGMA(J) = ACNORM(J)
         WA(J) = SIGMA(J)
         IF (PIVOT) IPVT(J) = J
   10    CONTINUE
C
C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.
C
      MINMN = MIN(M,N)
      DO 110 J = 1, MINMN
         IF (.NOT.PIVOT) GO TO 40
C
C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.
C
         KMAX = J
         DO 20 K = J, N
            IF (SIGMA(K) .GT. SIGMA(KMAX)) KMAX = K
   20       CONTINUE
         IF (KMAX .EQ. J) GO TO 40
         DO 30 I = 1, M
            TEMP = A(I,J)
            A(I,J) = A(I,KMAX)
            A(I,KMAX) = TEMP
   30       CONTINUE
         SIGMA(KMAX) = SIGMA(J)
         WA(KMAX) = WA(J)
         K = IPVT(J)
         IPVT(J) = IPVT(KMAX)
         IPVT(KMAX) = K
   40    CONTINUE
C
C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE
C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.
C
         AJNORM = DENORM(M-J+1,A(J,J))
         IF (AJNORM .EQ. ZERO) GO TO 100
         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM
         DO 50 I = J, M
            A(I,J) = A(I,J)/AJNORM
   50       CONTINUE
         A(J,J) = A(J,J) + ONE
C
C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS
C        AND UPDATE THE NORMS.
C
         JP1 = J + 1
         IF (N .LT. JP1) GO TO 100
         DO 90 K = JP1, N
            SUM = ZERO
            DO 60 I = J, M
               SUM = SUM + A(I,J)*A(I,K)
   60          CONTINUE
            TEMP = SUM/A(J,J)
            DO 70 I = J, M
               A(I,K) = A(I,K) - TEMP*A(I,J)
   70          CONTINUE
            IF (.NOT.PIVOT .OR. SIGMA(K) .EQ. ZERO) GO TO 80
            TEMP = A(J,K)/SIGMA(K)
            SIGMA(K) = SIGMA(K)*SQRT(MAX(ZERO,ONE-TEMP**2))
            IF (P05*(SIGMA(K)/WA(K))**2 .GT. EPSMCH) GO TO 80
            SIGMA(K) = DENORM(M-J,A(JP1,K))
            WA(K) = SIGMA(K)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
         SIGMA(J) = -AJNORM
  110    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DQRFAC.
C
      END
