      SUBROUTINE REBAKB (NM, N, B, DL, M, Z)
C
      INTEGER I,J,K,M,N,I1,II,NM
      REAL B(NM,*),DL(*),Z(NM,*)
      REAL X
C
C***FIRST EXECUTABLE STATEMENT  REBAKB
      IF (M .EQ. 0) GO TO 200
C
      DO 100 J = 1, M
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
         DO 100 II = 1, N
            I1 = N - II
            I = I1 + 1
            X = DL(I) * Z(I,J)
            IF (I .EQ. 1) GO TO 80
C
            DO 60 K = 1, I1
   60       X = X + B(I,K) * Z(K,J)
C
   80       Z(I,J) = X
  100 CONTINUE
C
  200 RETURN
      END
