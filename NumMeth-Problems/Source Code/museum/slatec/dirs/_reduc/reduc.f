      SUBROUTINE REDUC (NM, N, A, B, DL, IERR)
C
      INTEGER I,J,K,N,I1,J1,NM,NN,IERR
      REAL A(NM,*),B(NM,*),DL(*)
      REAL X,Y
C
C***FIRST EXECUTABLE STATEMENT  REDUC
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
C     .......... FORM THE TRANSPOSE OF THE UPPER TRIANGLE OF INV(L)*A
C                IN THE LOWER TRIANGLE OF THE ARRAY A ..........
  100 DO 200 I = 1, NN
         I1 = I - 1
         Y = DL(I)
C
         DO 200 J = I, NN
            X = A(I,J)
            IF (I .EQ. 1) GO TO 180
C
            DO 160 K = 1, I1
  160       X = X - B(I,K) * A(J,K)
C
  180       A(J,I) = X / Y
  200 CONTINUE
C     .......... PRE-MULTIPLY BY INV(L) AND OVERWRITE ..........
      DO 300 J = 1, NN
         J1 = J - 1
C
         DO 300 I = J, NN
            X = A(I,J)
            IF (I .EQ. J) GO TO 240
            I1 = I - 1
C
            DO 220 K = J, I1
  220       X = X - A(K,J) * B(I,K)
C
  240       IF (J .EQ. 1) GO TO 280
C
            DO 260 K = 1, J1
  260       X = X - A(J,K) * B(I,K)
C
  280       A(I,J) = X / DL(I)
  300 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- B IS NOT POSITIVE DEFINITE ..........
 1000 IERR = 7 * N + 1
 1001 RETURN
      END
