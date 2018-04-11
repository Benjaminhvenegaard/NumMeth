      SUBROUTINE REDUC2 (NM, N, A, B, DL, IERR)
C
      INTEGER I,J,K,N,I1,J1,NM,NN,IERR
      REAL A(NM,*),B(NM,*),DL(*)
      REAL X,Y
C
C***FIRST EXECUTABLE STATEMENT  REDUC2
      IERR = 0
      NN = ABS(N)
      IF (N .LT. 0) GO TO 100
C     .......... FORM L IN THE ARRAYS B AND DL ..........
      DO 80 I = 1, N
         I1 = I - 1
C
         DO 80 J = I, N
            X = B(I,J)
            IF (I .EQ. 1) GO TO 40
C
            DO 20 K = 1, I1
   20       X = X - B(I,K) * B(J,K)
C
   40       IF (J .NE. I) GO TO 60
            IF (X .LE. 0.0E0) GO TO 1000
            Y = SQRT(X)
            DL(I) = Y
            GO TO 80
   60       B(J,I) = X / Y
   80 CONTINUE
C     .......... FORM THE LOWER TRIANGLE OF A*L
C                IN THE LOWER TRIANGLE OF THE ARRAY A ..........
  100 DO 200 I = 1, NN
         I1 = I + 1
C
         DO 200 J = 1, I
            X = A(J,I) * DL(J)
            IF (J .EQ. I) GO TO 140
            J1 = J + 1
C
            DO 120 K = J1, I
  120       X = X + A(K,I) * B(K,J)
C
  140       IF (I .EQ. NN) GO TO 180
C
            DO 160 K = I1, NN
  160       X = X + A(I,K) * B(K,J)
C
  180       A(I,J) = X
  200 CONTINUE
C     .......... PRE-MULTIPLY BY TRANSPOSE(L) AND OVERWRITE ..........
      DO 300 I = 1, NN
         I1 = I + 1
         Y = DL(I)
C
         DO 300 J = 1, I
            X = Y * A(I,J)
            IF (I .EQ. NN) GO TO 280
C
            DO 260 K = I1, NN
  260       X = X + A(K,J) * B(K,I)
C
  280       A(I,J) = X
  300 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- B IS NOT POSITIVE DEFINITE ..........
 1000 IERR = 7 * N + 1
 1001 RETURN
      END
