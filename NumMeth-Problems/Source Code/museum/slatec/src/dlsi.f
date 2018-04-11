      SUBROUTINE DLSI (W, MDW, MA, MG, N, PRGOPT, X, RNORM, MODE, WS,
     +   IP)
      INTEGER IP(*), MA, MDW, MG, MODE, N
      DOUBLE PRECISION PRGOPT(*), RNORM, W(MDW,*), WS(*), X(*)
C
      EXTERNAL D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DHFTI, DLPDP,
     *   DSCAL, DSWAP
      DOUBLE PRECISION D1MACH, DASUM, DDOT
C
      DOUBLE PRECISION ANORM, DRELPR, FAC, GAM, RB, TAU, TOL, XNORM
      INTEGER I, J, K, KEY, KRANK, KRM1, KRP1, L, LAST, LINK, M, MAP1,
     *   MDLPDP, MINMAN, N1, N2, N3, NEXT, NP1
      LOGICAL COV, FIRST, SCLCOV
C
      SAVE DRELPR, FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  DLSI
C
C     Set the nominal tolerance used in the code.
C
      IF (FIRST) DRELPR = D1MACH(4)
      FIRST = .FALSE.
      TOL = SQRT(DRELPR)
C
      MODE = 0
      RNORM = 0.D0
      M = MA + MG
      NP1 = N + 1
      KRANK = 0
      IF (N.LE.0 .OR. M.LE.0) GO TO 370
C
C     To process option vector.
C
      COV = .FALSE.
      SCLCOV = .TRUE.
      LAST = 1
      LINK = PRGOPT(1)
C
  100 IF (LINK.GT.1) THEN
         KEY = PRGOPT(LAST+1)
         IF (KEY.EQ.1) COV = PRGOPT(LAST+2) .NE. 0.D0
         IF (KEY.EQ.10) SCLCOV = PRGOPT(LAST+2) .EQ. 0.D0
         IF (KEY.EQ.5) TOL = MAX(DRELPR,PRGOPT(LAST+2))
         NEXT = PRGOPT(LINK)
         LAST = LINK
         LINK = NEXT
         GO TO 100
      ENDIF
C
C     Compute matrix norm of least squares equations.
C
      ANORM = 0.D0
      DO 110 J = 1,N
         ANORM = MAX(ANORM,DASUM(MA,W(1,J),1))
  110 CONTINUE
C
C     Set tolerance for DHFTI( ) rank test.
C
      TAU = TOL*ANORM
C
C     Compute Householder orthogonal decomposition of matrix.
C
      CALL DCOPY (N, 0.D0, 0, WS, 1)
      CALL DCOPY (MA, W(1, NP1), 1, WS, 1)
      K = MAX(M,N)
      MINMAN = MIN(MA,N)
      N1 = K + 1
      N2 = N1 + N
      CALL DHFTI (W, MDW, MA, N, WS, MA, 1, TAU, KRANK, RNORM, WS(N2),
     +           WS(N1), IP)
      FAC = 1.D0
      GAM = MA - KRANK
      IF (KRANK.LT.MA .AND. SCLCOV) FAC = RNORM**2/GAM
C
C     Reduce to DLPDP and solve.
C
      MAP1 = MA + 1
C
C     Compute inequality rt-hand side for DLPDP.
C
      IF (MA.LT.M) THEN
         IF (MINMAN.GT.0) THEN
            DO 120 I = MAP1,M
               W(I,NP1) = W(I,NP1) - DDOT(N,W(I,1),MDW,WS,1)
  120       CONTINUE
C
C           Apply permutations to col. of inequality constraint matrix.
C
            DO 130 I = 1,MINMAN
               CALL DSWAP (MG, W(MAP1,I), 1, W(MAP1,IP(I)), 1)
  130       CONTINUE
C
C           Apply Householder transformations to constraint matrix.
C
            IF (KRANK.GT.0 .AND. KRANK.LT.N) THEN
               DO 140 I = KRANK,1,-1
                  CALL DH12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
     +                      W(MAP1,1), MDW, 1, MG)
  140          CONTINUE
            ENDIF
C
C           Compute permuted inequality constraint matrix times r-inv.
C
            DO 160 I = MAP1,M
               DO 150 J = 1,KRANK
                  W(I,J) = (W(I,J)-DDOT(J-1,W(1,J),1,W(I,1),MDW))/W(J,J)
  150          CONTINUE
  160       CONTINUE
         ENDIF
C
C        Solve the reduced problem with DLPDP algorithm,
C        the least projected distance problem.
C
         CALL DLPDP(W(MAP1,1), MDW, MG, KRANK, N-KRANK, PRGOPT, X,
     +             XNORM, MDLPDP, WS(N2), IP(N+1))
C
C        Compute solution in original coordinates.
C
         IF (MDLPDP.EQ.1) THEN
            DO 170 I = KRANK,1,-1
               X(I) = (X(I)-DDOT(KRANK-I,W(I,I+1),MDW,X(I+1),1))/W(I,I)
  170       CONTINUE
C
C           Apply Householder transformation to solution vector.
C
            IF (KRANK.LT.N) THEN
               DO 180 I = 1,KRANK
                  CALL DH12 (2, I, KRANK+1, N, W(I,1), MDW, WS(N1+I-1),
     +                      X, 1, 1, 1)
  180          CONTINUE
            ENDIF
C
C           Repermute variables to their input order.
C
            IF (MINMAN.GT.0) THEN
               DO 190 I = MINMAN,1,-1
                  CALL DSWAP (1, X(I), 1, X(IP(I)), 1)
  190          CONTINUE
C
C              Variables are now in original coordinates.
C              Add solution of unconstrained problem.
C
               DO 200 I = 1,N
                  X(I) = X(I) + WS(I)
  200          CONTINUE
C
C              Compute the residual vector norm.
C
               RNORM = SQRT(RNORM**2+XNORM**2)
            ENDIF
         ELSE
            MODE = 2
         ENDIF
      ELSE
         CALL DCOPY (N, WS, 1, X, 1)
      ENDIF
C
C     Compute covariance matrix based on the orthogonal decomposition
C     from DHFTI( ).
C
      IF (.NOT.COV .OR. KRANK.LE.0) GO TO 370
      KRM1 = KRANK - 1
      KRP1 = KRANK + 1
C
C     Copy diagonal terms to working array.
C
      CALL DCOPY (KRANK, W, MDW+1, WS(N2), 1)
C
C     Reciprocate diagonal terms.
C
      DO 210 J = 1,KRANK
         W(J,J) = 1.D0/W(J,J)
  210 CONTINUE
C
C     Invert the upper triangular QR factor on itself.
C
      IF (KRANK.GT.1) THEN
         DO 230 I = 1,KRM1
            DO 220 J = I+1,KRANK
               W(I,J) = -DDOT(J-I,W(I,I),MDW,W(I,J),1)*W(J,J)
  220       CONTINUE
  230    CONTINUE
      ENDIF
C
C     Compute the inverted factor times its transpose.
C
      DO 250 I = 1,KRANK
         DO 240 J = I,KRANK
            W(I,J) = DDOT(KRANK+1-J,W(I,J),MDW,W(J,J),MDW)
  240    CONTINUE
  250 CONTINUE
C
C     Zero out lower trapezoidal part.
C     Copy upper triangular to lower triangular part.
C
      IF (KRANK.LT.N) THEN
         DO 260 J = 1,KRANK
            CALL DCOPY (J, W(1,J), 1, W(J,1), MDW)
  260    CONTINUE
C
         DO 270 I = KRP1,N
            CALL DCOPY (I, 0.D0, 0, W(I,1), MDW)
  270    CONTINUE
C
C        Apply right side transformations to lower triangle.
C
         N3 = N2 + KRP1
         DO 330 I = 1,KRANK
            L = N1 + I
            K = N2 + I
            RB = WS(L-1)*WS(K-1)
C
C           If RB.GE.0.D0, transformation can be regarded as zero.
C
            IF (RB.LT.0.D0) THEN
               RB = 1.D0/RB
C
C              Store unscaled rank one Householder update in work array.
C
               CALL DCOPY (N, 0.D0, 0, WS(N3), 1)
               L = N1 + I
               K = N3 + I
               WS(K-1) = WS(L-1)
C
               DO 280 J = KRP1,N
                  WS(N3+J-1) = W(I,J)
  280          CONTINUE
C
               DO 290 J = 1,N
                  WS(J) = RB*(DDOT(J-I,W(J,I),MDW,WS(N3+I-1),1)+
     +                    DDOT(N-J+1,W(J,J),1,WS(N3+J-1),1))
  290          CONTINUE
C
               L = N3 + I
               GAM = 0.5D0*RB*DDOT(N-I+1,WS(L-1),1,WS(I),1)
               CALL DAXPY (N-I+1, GAM, WS(L-1), 1, WS(I), 1)
               DO 320 J = I,N
                  DO 300 L = 1,I-1
                     W(J,L) = W(J,L) + WS(N3+J-1)*WS(L)
  300             CONTINUE
C
                  DO 310 L = I,J
                     W(J,L) = W(J,L) + WS(J)*WS(N3+L-1)+WS(L)*WS(N3+J-1)
  310             CONTINUE
  320          CONTINUE
            ENDIF
  330    CONTINUE
C
C        Copy lower triangle to upper triangle to symmetrize the
C        covariance matrix.
C
         DO 340 I = 1,N
            CALL DCOPY (I, W(I,1), MDW, W(1,I), 1)
  340    CONTINUE
      ENDIF
C
C     Repermute rows and columns.
C
      DO 350 I = MINMAN,1,-1
         K = IP(I)
         IF (I.NE.K) THEN
            CALL DSWAP (1, W(I,I), 1, W(K,K), 1)
            CALL DSWAP (I-1, W(1,I), 1, W(1,K), 1)
            CALL DSWAP (K-I-1, W(I,I+1), MDW, W(I+1,K), 1)
            CALL DSWAP (N-K, W(I, K+1), MDW, W(K, K+1), MDW)
         ENDIF
  350 CONTINUE
C
C     Put in normalized residual sum of squares scale factor
C     and symmetrize the resulting covariance matrix.
C
      DO 360 J = 1,N
         CALL DSCAL (J, FAC, W(1,J), 1)
         CALL DCOPY (J, W(1,J), 1, W(J,1), MDW)
  360 CONTINUE
C
  370 IP(1) = KRANK
      IP(2) = N + MAX(M,N) + (MG+2)*(N+7)
      RETURN
      END