      SUBROUTINE DLSSUD (A, X, B, N, M, NRDA, U, NRDU, IFLAG, MLSO,
     +   IRANK, ISCALE, Q, DIAG, KPIVOT, S, DIV, TD, ISFLG, SCALES)
      INTEGER J4SAVE
      DOUBLE PRECISION DDOT, D1MACH
      INTEGER I, IFLAG, IRANK, IRP, ISCALE, ISFLG, J, JR, K, KP,
     1     KPIVOT(*), L, M, MAXMES, MJ, MLSO, N, NFAT, NFATAL, NMIR,
     2     NRDA, NRDU, NU
      DOUBLE PRECISION A(NRDA,*), B(*), DIAG(*), DIV(*), GAM, GAMMA,
     1     Q(NRDA,*), RES, S(*), SCALES(*), SS, TD(*), U(NRDU,*), URO,
     2     X(*)
C
C     ******************************************************************
C
C          MACHINE PRECISION (COMPUTER UNIT ROUNDOFF VALUE) IS DEFINED
C          BY THE FUNCTION D1MACH.
C
C     ******************************************************************
C
C     BEGIN BLOCK PERMITTING ...EXITS TO 310
C        BEGIN BLOCK PERMITTING ...EXITS TO 80
C***FIRST EXECUTABLE STATEMENT  DLSSUD
            URO = D1MACH(4)
C
            IF (N .LT. 1 .OR. M .LT. N .OR. NRDA .LT. N) GO TO 70
            IF (NRDU .NE. 0 .AND. NRDU .LT. M) GO TO 70
               IF (IFLAG .GT. 0) GO TO 60
C
                  CALL XGETF(NFATAL)
                  MAXMES = J4SAVE(4,0,.FALSE.)
                  ISFLG = -15
                  IF (IFLAG .EQ. 0) GO TO 10
                     ISFLG = IFLAG
                     NFAT = -1
                     IF (NFATAL .EQ. 0) NFAT = 0
                     CALL XSETF(NFAT)
                     CALL XERMAX(1)
   10             CONTINUE
C
C                 COPY MATRIX A INTO MATRIX Q
C
                  DO 30 K = 1, M
                     DO 20 J = 1, N
                        Q(J,K) = A(J,K)
   20                CONTINUE
   30             CONTINUE
C
C                 USE ORTHOGONAL TRANSFORMATIONS TO REDUCE Q TO LOWER
C                 TRIANGULAR FORM
C
                  CALL DORTHR(Q,N,M,NRDA,IFLAG,IRANK,ISCALE,DIAG,KPIVOT,
     1                        SCALES,DIV,TD)
C
                  CALL XSETF(NFATAL)
                  CALL XERMAX(MAXMES)
                  IF (IRANK .EQ. N) GO TO 40
C
C                    FOR RANK DEFICIENT PROBLEMS USE ADDITIONAL
C                    ORTHOGONAL TRANSFORMATIONS TO FURTHER REDUCE Q
C
                     IF (IRANK .NE. 0)
     1                  CALL DOHTRL(Q,N,NRDA,DIAG,IRANK,DIV,TD)
C     ...............EXIT
                     GO TO 310
   40             CONTINUE
C
C                 STORE DIVISORS FOR THE TRIANGULAR SOLUTION
C
                  DO 50 K = 1, N
                     DIV(K) = DIAG(K)
   50             CONTINUE
C        .........EXIT
                  GO TO 80
   60          CONTINUE
C        ......EXIT
               IF (IFLAG .EQ. 1) GO TO 80
   70       CONTINUE
C
C           INVALID INPUT FOR DLSSUD
            IFLAG = 2
            CALL XERMSG ('SLATEC', 'DLSSUD',
     +         'INVALID IMPUT PARAMETERS.', 2, 1)
C     ......EXIT
            GO TO 310
   80    CONTINUE
C
C
         IF (IRANK .GT. 0) GO TO 130
C
C           SPECIAL CASE FOR THE NULL MATRIX
            DO 110 K = 1, M
               X(K) = 0.0D0
               IF (MLSO .EQ. 0) GO TO 100
                  U(K,K) = 1.0D0
                  DO 90 J = 1, M
                     IF (J .NE. K) U(J,K) = 0.0D0
   90             CONTINUE
  100          CONTINUE
  110       CONTINUE
            DO 120 K = 1, N
               IF (B(K) .GT. 0.0D0) IFLAG = 4
  120       CONTINUE
         GO TO 300
  130    CONTINUE
C           BEGIN BLOCK PERMITTING ...EXITS TO 180
C
C              COPY CONSTANT VECTOR INTO S AFTER FIRST INTERCHANGING
C              THE ELEMENTS ACCORDING TO THE PIVOTAL SEQUENCE
C
               DO 140 K = 1, N
                  KP = KPIVOT(K)
                  X(K) = B(KP)
  140          CONTINUE
               DO 150 K = 1, N
                  S(K) = X(K)
  150          CONTINUE
C
               IRP = IRANK + 1
               NU = 1
               IF (MLSO .EQ. 0) NU = 0
C           ...EXIT
               IF (IRANK .EQ. N) GO TO 180
C
C              FOR RANK DEFICIENT PROBLEMS WE MUST APPLY THE
C              ORTHOGONAL TRANSFORMATION TO S
C              WE ALSO CHECK TO SEE IF THE SYSTEM APPEARS TO BE
C              INCONSISTENT
C
               NMIR = N - IRANK
               SS = DDOT(N,S(1),1,S(1),1)
               DO 170 L = 1, IRANK
                  K = IRP - L
                  GAM = ((TD(K)*S(K)) + DDOT(NMIR,Q(IRP,K),1,S(IRP),1))
     1                  /(TD(K)*DIV(K))
                  S(K) = S(K) + GAM*TD(K)
                  DO 160 J = IRP, N
                     S(J) = S(J) + GAM*Q(J,K)
  160             CONTINUE
  170          CONTINUE
               RES = DDOT(NMIR,S(IRP),1,S(IRP),1)
C           ...EXIT
               IF (RES
     1             .LE. SS*(10.0D0*MAX(10.0D0**ISFLG,10.0D0*URO))**2)
     2            GO TO 180
C
C              INCONSISTENT SYSTEM
               IFLAG = 4
               NU = 0
  180       CONTINUE
C
C           APPLY FORWARD SUBSTITUTION TO SOLVE LOWER TRIANGULAR SYSTEM
C
            S(1) = S(1)/DIV(1)
            IF (IRANK .LT. 2) GO TO 200
            DO 190 K = 2, IRANK
               S(K) = (S(K) - DDOT(K-1,Q(K,1),NRDA,S(1),1))/DIV(K)
  190       CONTINUE
  200       CONTINUE
C
C           INITIALIZE X VECTOR AND THEN APPLY ORTHOGONAL TRANSFORMATION
C
            DO 210 K = 1, M
               X(K) = 0.0D0
               IF (K .LE. IRANK) X(K) = S(K)
  210       CONTINUE
C
            DO 230 JR = 1, IRANK
               J = IRP - JR
               MJ = M - J + 1
               GAMMA = DDOT(MJ,Q(J,J),NRDA,X(J),1)/(DIAG(J)*Q(J,J))
               DO 220 K = J, M
                  X(K) = X(K) + GAMMA*Q(J,K)
  220          CONTINUE
  230       CONTINUE
C
C           RESCALE ANSWERS AS DICTATED
C
            DO 240 K = 1, M
               X(K) = X(K)*SCALES(K)
  240       CONTINUE
C
            IF (NU .EQ. 0 .OR. M .EQ. IRANK) GO TO 290
C
C              INITIALIZE U MATRIX AND THEN APPLY ORTHOGONAL
C              TRANSFORMATION
C
               L = M - IRANK
               DO 280 K = 1, L
                  DO 250 I = 1, M
                     U(I,K) = 0.0D0
                     IF (I .EQ. IRANK + K) U(I,K) = 1.0D0
  250             CONTINUE
C
                  DO 270 JR = 1, IRANK
                     J = IRP - JR
                     MJ = M - J + 1
                     GAMMA = DDOT(MJ,Q(J,J),NRDA,U(J,K),1)
     1                       /(DIAG(J)*Q(J,J))
                     DO 260 I = J, M
                        U(I,K) = U(I,K) + GAMMA*Q(J,I)
  260                CONTINUE
  270             CONTINUE
  280          CONTINUE
  290       CONTINUE
  300    CONTINUE
  310 CONTINUE
C
      RETURN
      END
