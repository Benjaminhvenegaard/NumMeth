      SUBROUTINE CQRSL (X, LDX, N, K, QRAUX, Y, QY, QTY, B, RSD, XB,
     +   JOB, INFO)
      INTEGER LDX,N,K,JOB,INFO
      COMPLEX X(LDX,*),QRAUX(*),Y(*),QY(*),QTY(*),B(*),RSD(*),XB(*)
C
      INTEGER I,J,JJ,JU,KP1
      COMPLEX CDOTC,T,TEMP
      LOGICAL CB,CQY,CQTY,CR,CXB
      COMPLEX ZDUM
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
C***FIRST EXECUTABLE STATEMENT  CQRSL
C
C     SET INFO FLAG.
C
      INFO = 0
C
C     DETERMINE WHAT IS TO BE COMPUTED.
C
      CQY = JOB/10000 .NE. 0
      CQTY = MOD(JOB,10000) .NE. 0
      CB = MOD(JOB,1000)/100 .NE. 0
      CR = MOD(JOB,100)/10 .NE. 0
      CXB = MOD(JOB,10) .NE. 0
      JU = MIN(K,N-1)
C
C     SPECIAL ACTION WHEN N=1.
C
      IF (JU .NE. 0) GO TO 40
         IF (CQY) QY(1) = Y(1)
         IF (CQTY) QTY(1) = Y(1)
         IF (CXB) XB(1) = Y(1)
         IF (.NOT.CB) GO TO 30
            IF (CABS1(X(1,1)) .NE. 0.0E0) GO TO 10
               INFO = 1
            GO TO 20
   10       CONTINUE
               B(1) = Y(1)/X(1,1)
   20       CONTINUE
   30    CONTINUE
         IF (CR) RSD(1) = (0.0E0,0.0E0)
      GO TO 250
   40 CONTINUE
C
C        SET UP TO COMPUTE QY OR QTY.
C
         IF (CQY) CALL CCOPY(N,Y,1,QY,1)
         IF (CQTY) CALL CCOPY(N,Y,1,QTY,1)
         IF (.NOT.CQY) GO TO 70
C
C           COMPUTE QY.
C
            DO 60 JJ = 1, JU
               J = JU - JJ + 1
               IF (CABS1(QRAUX(J)) .EQ. 0.0E0) GO TO 50
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  T = -CDOTC(N-J+1,X(J,J),1,QY(J),1)/X(J,J)
                  CALL CAXPY(N-J+1,T,X(J,J),1,QY(J),1)
                  X(J,J) = TEMP
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
         IF (.NOT.CQTY) GO TO 100
C
C           COMPUTE CTRANS(Q)*Y.
C
            DO 90 J = 1, JU
               IF (CABS1(QRAUX(J)) .EQ. 0.0E0) GO TO 80
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  T = -CDOTC(N-J+1,X(J,J),1,QTY(J),1)/X(J,J)
                  CALL CAXPY(N-J+1,T,X(J,J),1,QTY(J),1)
                  X(J,J) = TEMP
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        SET UP TO COMPUTE B, RSD, OR XB.
C
         IF (CB) CALL CCOPY(K,QTY,1,B,1)
         KP1 = K + 1
         IF (CXB) CALL CCOPY(K,QTY,1,XB,1)
         IF (CR .AND. K .LT. N) CALL CCOPY(N-K,QTY(KP1),1,RSD(KP1),1)
         IF (.NOT.CXB .OR. KP1 .GT. N) GO TO 120
            DO 110 I = KP1, N
               XB(I) = (0.0E0,0.0E0)
  110       CONTINUE
  120    CONTINUE
         IF (.NOT.CR) GO TO 140
            DO 130 I = 1, K
               RSD(I) = (0.0E0,0.0E0)
  130       CONTINUE
  140    CONTINUE
         IF (.NOT.CB) GO TO 190
C
C           COMPUTE B.
C
            DO 170 JJ = 1, K
               J = K - JJ + 1
               IF (CABS1(X(J,J)) .NE. 0.0E0) GO TO 150
                  INFO = J
                  GO TO 180
  150          CONTINUE
               B(J) = B(J)/X(J,J)
               IF (J .EQ. 1) GO TO 160
                  T = -B(J)
                  CALL CAXPY(J-1,T,X(1,J),1,B,1)
  160          CONTINUE
  170       CONTINUE
  180       CONTINUE
  190    CONTINUE
         IF (.NOT.CR .AND. .NOT.CXB) GO TO 240
C
C           COMPUTE RSD OR XB AS REQUIRED.
C
            DO 230 JJ = 1, JU
               J = JU - JJ + 1
               IF (CABS1(QRAUX(J)) .EQ. 0.0E0) GO TO 220
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  IF (.NOT.CR) GO TO 200
                     T = -CDOTC(N-J+1,X(J,J),1,RSD(J),1)/X(J,J)
                     CALL CAXPY(N-J+1,T,X(J,J),1,RSD(J),1)
  200             CONTINUE
                  IF (.NOT.CXB) GO TO 210
                     T = -CDOTC(N-J+1,X(J,J),1,XB(J),1)/X(J,J)
                     CALL CAXPY(N-J+1,T,X(J,J),1,XB(J),1)
  210             CONTINUE
                  X(J,J) = TEMP
  220          CONTINUE
  230       CONTINUE
  240    CONTINUE
  250 CONTINUE
      RETURN
      END
