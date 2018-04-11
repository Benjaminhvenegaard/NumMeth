      SUBROUTINE ORTRAN (NM, N, LOW, IGH, A, ORT, Z)
C
      INTEGER I,J,N,KL,MM,MP,NM,IGH,LOW,MP1
      REAL A(NM,*),ORT(*),Z(NM,*)
      REAL G
C
C     .......... INITIALIZE Z TO IDENTITY MATRIX ..........
C***FIRST EXECUTABLE STATEMENT  ORTRAN
      DO 80 I = 1, N
C
         DO 60 J = 1, N
   60    Z(I,J) = 0.0E0
C
         Z(I,I) = 1.0E0
   80 CONTINUE
C
      KL = IGH - LOW - 1
      IF (KL .LT. 1) GO TO 200
C     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
      DO 140 MM = 1, KL
         MP = IGH - MM
         IF (A(MP,MP-1) .EQ. 0.0E0) GO TO 140
         MP1 = MP + 1
C
         DO 100 I = MP1, IGH
  100    ORT(I) = A(I,MP-1)
C
         DO 130 J = MP, IGH
            G = 0.0E0
C
            DO 110 I = MP, IGH
  110       G = G + ORT(I) * Z(I,J)
C     .......... DIVISOR BELOW IS NEGATIVE OF H FORMED IN ORTHES.
C                DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
            G = (G / ORT(MP)) / A(MP,MP-1)
C
            DO 120 I = MP, IGH
  120       Z(I,J) = Z(I,J) + G * ORT(I)
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
