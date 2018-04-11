      SUBROUTINE CQRDC (X, LDX, N, P, QRAUX, JPVT, WORK, JOB)
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      COMPLEX X(LDX,*),QRAUX(*),WORK(*)
C
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      REAL MAXNRM,SCNRM2,TT
      COMPLEX CDOTC,NRMXL,T
      LOGICAL NEGJ,SWAPJ
      COMPLEX CSIGN,ZDUM,ZDUM1,ZDUM2
      REAL CABS1
      CSIGN(ZDUM1,ZDUM2) = ABS(ZDUM1)*(ZDUM2/ABS(ZDUM2))
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
C
C***FIRST EXECUTABLE STATEMENT  CQRDC
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GO TO 60
C
C        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
C        ACCORDING TO JPVT.
C
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GO TO 10
               IF (J .NE. PL) CALL CSWAP(N,X(1,PL),1,X(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GO TO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GO TO 30
                  CALL CSWAP(N,X(1,PU),1,X(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
C
C     COMPUTE THE NORMS OF THE FREE COLUMNS.
C
      IF (PU .LT. PL) GO TO 80
      DO 70 J = PL, PU
         QRAUX(J) = CMPLX(SCNRM2(N,X(1,J),1),0.0E0)
         WORK(J) = QRAUX(J)
   70 CONTINUE
   80 CONTINUE
C
C     PERFORM THE HOUSEHOLDER REDUCTION OF X.
C
      LUP = MIN(N,P)
      DO 200 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GO TO 120
C
C           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
C           INTO THE PIVOT POSITION.
C
            MAXNRM = 0.0E0
            MAXJ = L
            DO 100 J = L, PU
               IF (REAL(QRAUX(J)) .LE. MAXNRM) GO TO 90
                  MAXNRM = REAL(QRAUX(J))
                  MAXJ = J
   90          CONTINUE
  100       CONTINUE
            IF (MAXJ .EQ. L) GO TO 110
               CALL CSWAP(N,X(1,L),1,X(1,MAXJ),1)
               QRAUX(MAXJ) = QRAUX(L)
               WORK(MAXJ) = WORK(L)
               JP = JPVT(MAXJ)
               JPVT(MAXJ) = JPVT(L)
               JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUX(L) = (0.0E0,0.0E0)
         IF (L .EQ. N) GO TO 190
C
C           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
C
            NRMXL = CMPLX(SCNRM2(N-L+1,X(L,L),1),0.0E0)
            IF (CABS1(NRMXL) .EQ. 0.0E0) GO TO 180
               IF (CABS1(X(L,L)) .NE. 0.0E0)
     1            NRMXL = CSIGN(NRMXL,X(L,L))
               CALL CSCAL(N-L+1,(1.0E0,0.0E0)/NRMXL,X(L,L),1)
               X(L,L) = (1.0E0,0.0E0) + X(L,L)
C
C              APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
C              UPDATING THE NORMS.
C
               LP1 = L + 1
               IF (P .LT. LP1) GO TO 170
               DO 160 J = LP1, P
                  T = -CDOTC(N-L+1,X(L,L),1,X(L,J),1)/X(L,L)
                  CALL CAXPY(N-L+1,T,X(L,L),1,X(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GO TO 150
                  IF (CABS1(QRAUX(J)) .EQ. 0.0E0) GO TO 150
                     TT = 1.0E0 - (ABS(X(L,J))/REAL(QRAUX(J)))**2
                     TT = MAX(TT,0.0E0)
                     T = CMPLX(TT,0.0E0)
                     TT = 1.0E0
     1                    + 0.05E0*TT*(REAL(QRAUX(J))/REAL(WORK(J)))**2
                     IF (TT .EQ. 1.0E0) GO TO 130
                        QRAUX(J) = QRAUX(J)*SQRT(T)
                     GO TO 140
  130                CONTINUE
                        QRAUX(J) = CMPLX(SCNRM2(N-L,X(L+1,J),1),0.0E0)
                        WORK(J) = QRAUX(J)
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
  170          CONTINUE
C
C              SAVE THE TRANSFORMATION.
C
               QRAUX(L) = X(L,L)
               X(L,L) = -NRMXL
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
      RETURN
      END
