      SUBROUTINE CTRSL (T, LDT, N, B, JOB, INFO)
      INTEGER LDT,N,JOB,INFO
      COMPLEX T(LDT,*),B(*)
C
C
      COMPLEX CDOTC,TEMP
      INTEGER CASE,J,JJ
      COMPLEX ZDUM
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
C***FIRST EXECUTABLE STATEMENT  CTRSL
C
C        CHECK FOR ZERO DIAGONAL ELEMENTS.
C
         DO 10 INFO = 1, N
            IF (CABS1(T(INFO,INFO)) .EQ. 0.0E0) GO TO 150
   10    CONTINUE
         INFO = 0
C
C        DETERMINE THE TASK AND GO TO IT.
C
         CASE = 1
         IF (MOD(JOB,10) .NE. 0) CASE = 2
         IF (MOD(JOB,100)/10 .NE. 0) CASE = CASE + 2
         GO TO (20,50,80,110), CASE
C
C        SOLVE T*X=B FOR T LOWER TRIANGULAR
C
   20    CONTINUE
            B(1) = B(1)/T(1,1)
            IF (N .LT. 2) GO TO 40
            DO 30 J = 2, N
               TEMP = -B(J-1)
               CALL CAXPY(N-J+1,TEMP,T(J,J-1),1,B(J),1)
               B(J) = B(J)/T(J,J)
   30       CONTINUE
   40       CONTINUE
         GO TO 140
C
C        SOLVE T*X=B FOR T UPPER TRIANGULAR.
C
   50    CONTINUE
            B(N) = B(N)/T(N,N)
            IF (N .LT. 2) GO TO 70
            DO 60 JJ = 2, N
               J = N - JJ + 1
               TEMP = -B(J+1)
               CALL CAXPY(J,TEMP,T(1,J+1),1,B(1),1)
               B(J) = B(J)/T(J,J)
   60       CONTINUE
   70       CONTINUE
         GO TO 140
C
C        SOLVE CTRANS(T)*X=B FOR T LOWER TRIANGULAR.
C
   80    CONTINUE
            B(N) = B(N)/CONJG(T(N,N))
            IF (N .LT. 2) GO TO 100
            DO 90 JJ = 2, N
               J = N - JJ + 1
               B(J) = B(J) - CDOTC(JJ-1,T(J+1,J),1,B(J+1),1)
               B(J) = B(J)/CONJG(T(J,J))
   90       CONTINUE
  100       CONTINUE
         GO TO 140
C
C        SOLVE CTRANS(T)*X=B FOR T UPPER TRIANGULAR.
C
  110    CONTINUE
            B(1) = B(1)/CONJG(T(1,1))
            IF (N .LT. 2) GO TO 130
            DO 120 J = 2, N
               B(J) = B(J) - CDOTC(J-1,T(1,J),1,B(1),1)
               B(J) = B(J)/CONJG(T(J,J))
  120       CONTINUE
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END
