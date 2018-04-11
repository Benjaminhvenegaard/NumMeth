      SUBROUTINE DBNDSL (MODE, G, MDG, NB, IP, IR, X, N, RNORM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION G(MDG,*),X(*)
C***FIRST EXECUTABLE STATEMENT  DBNDSL
      ZERO=0.D0
C
      RNORM=ZERO
      GO TO (10,90,50), MODE
C                                   ********************* MODE = 1
C                                   ALG. STEP 26
   10      DO 20 J=1,N
           X(J)=G(J,NB+1)
   20 CONTINUE
      RSQ=ZERO
      NP1=N+1
      IRM1=IR-1
      IF (NP1.GT.IRM1) GO TO 40
           DO 30 J=NP1,IRM1
           RSQ=RSQ+G(J,NB+1)**2
   30 CONTINUE
      RNORM=SQRT(RSQ)
   40 CONTINUE
C                                   ********************* MODE = 3
C                                   ALG. STEP 27
   50      DO 80 II=1,N
           I=N+1-II
C                                   ALG. STEP 28
           S=ZERO
           L=MAX(0,I-IP)
C                                   ALG. STEP 29
           IF (I.EQ.N) GO TO 70
C                                   ALG. STEP 30
           IE=MIN(N+1-I,NB)
                DO 60 J=2,IE
                JG=J+L
                IX=I-1+J
                S=S+G(I,JG)*X(IX)
   60 CONTINUE
C                                   ALG. STEP 31
   70      IF (G(I,L+1)) 80,130,80
   80      X(I)=(X(I)-S)/G(I,L+1)
C                                   ALG. STEP 32
      RETURN
C                                   ********************* MODE = 2
   90      DO 120 J=1,N
           S=ZERO
           IF (J.EQ.1) GO TO 110
           I1=MAX(1,J-NB+1)
           I2=J-1
                DO 100 I=I1,I2
                L=J-I+1+MAX(0,I-IP)
                S=S+X(I)*G(I,L)
  100 CONTINUE
  110      L=MAX(0,J-IP)
           IF (G(J,L+1)) 120,130,120
  120      X(J)=(X(J)-S)/G(J,L+1)
      RETURN
C
  130 CONTINUE
      NERR=1
      IOPT=2
      CALL XERMSG ('SLATEC', 'DBNDSL',
     +   'A ZERO DIAGONAL TERM IS IN THE N BY N UPPER TRIANGULAR ' //
     +   'MATRIX.', NERR, IOPT)
      RETURN
      END
