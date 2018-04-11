      SUBROUTINE REBAK (NM, N, B, DL, M, Z)
C
      INTEGER I,J,K,M,N,I1,II,NM
      REAL B(NM,*),DL(*),Z(NM,*)
      REAL X
C
C***FIRST EXECUTABLE STATEMENT  REBAK
      IF (M .EQ. 0) GO TO 200
C
      DO 100 J = 1, M
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
         DO 100 II = 1, N
            I = N + 1 - II
            I1 = I + 1
            X = Z(I,J)
            IF (I .EQ. N) GO TO 80
C
            DO 60 K = I1, N
   60       X = X - B(K,I) * Z(K,J)
C
   80       Z(I,J) = X / DL(I)
  100 CONTINUE
C
  200 RETURN
      END
