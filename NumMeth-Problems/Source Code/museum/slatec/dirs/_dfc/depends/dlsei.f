      SUBROUTINE DLSEI (W, MDW, ME, MA, MG, N, PRGOPT, X, RNORME,
     +   RNORML, MODE, WS, IP)
      INTEGER IP(3), MA, MDW, ME, MG, MODE, N
      DOUBLE PRECISION PRGOPT(*), RNORME, RNORML, W(MDW,*), WS(*), X(*)
C
      EXTERNAL D1MACH, DASUM, DAXPY, DCOPY, DDOT, DH12, DLSI, DNRM2,
     *   DSCAL, DSWAP, XERMSG
      DOUBLE PRECISION D1MACH, DASUM, DDOT, DNRM2
C
      DOUBLE PRECISION DRELPR, ENORM, FNORM, GAM, RB, RN, RNMAX, SIZE,
     *   SN, SNMAX, T, TAU, UJ, UP, VJ, XNORM, XNRME
      INTEGER I, IMAX, J, JP1, K, KEY, KRANKE, LAST, LCHK, LINK, M,
     *   MAPKE1, MDEQC, MEND, MEP1, N1, N2, NEXT, NLINK, NOPT, NP1,
     *   NTIMES
      LOGICAL COV, FIRST
      CHARACTER*8 XERN1, XERN2, XERN3, XERN4
      SAVE FIRST, DRELPR
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLSEI
C
C     Set the nominal tolerance used in the code for the equality
C     constraint equations.
C
      IF (FIRST) DRELPR = D1MACH(4)
      FIRST = .FALSE.
      TAU = SQRT(DRELPR)
C
C     Check that enough storage was allocated in WS(*) and IP(*).
C
      MODE = 4
      IF (MIN(N,ME,MA,MG) .LT. 0) THEN
         WRITE (XERN1, '(I8)') N
         WRITE (XERN2, '(I8)') ME
         WRITE (XERN3, '(I8)') MA
         WRITE (XERN4, '(I8)') MG
         CALL XERMSG ('SLATEC', 'LSEI', 'ALL OF THE VARIABLES N, ME,' //
     *      ' MA, MG MUST BE .GE. 0$$ENTERED ROUTINE WITH' //
     *      '$$N  = ' // XERN1 //
     *      '$$ME = ' // XERN2 //
     *      '$$MA = ' // XERN3 //
     *      '$$MG = ' // XERN4, 2, 1)
         RETURN
      ENDIF
C
      IF (IP(1).GT.0) THEN
         LCHK = 2*(ME+N) + MAX(MA+MG,N) + (MG+2)*(N+7)
         IF (IP(1).LT.LCHK) THEN
            WRITE (XERN1, '(I8)') LCHK
            CALL XERMSG ('SLATEC', 'DLSEI', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR WS(*), NEED LW = ' // XERN1, 2, 1)
            RETURN
         ENDIF
      ENDIF
C
      IF (IP(2).GT.0) THEN
         LCHK = MG + 2*N + 2
         IF (IP(2).LT.LCHK) THEN
            WRITE (XERN1, '(I8)') LCHK
            CALL XERMSG ('SLATEC', 'DLSEI', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR IP(*), NEED LIP = ' // XERN1, 2, 1)
            RETURN
         ENDIF
      ENDIF
C
C     Compute number of possible right multiplying Householder
C     transformations.
C
      M = ME + MA + MG
      IF (N.LE.0 .OR. M.LE.0) THEN
         MODE = 0
         RNORME = 0
         RNORML = 0
         RETURN
      ENDIF
C
      IF (MDW.LT.M) THEN
         CALL XERMSG ('SLATEC', 'DLSEI', 'MDW.LT.ME+MA+MG IS AN ERROR',
     +      2, 1)
         RETURN
      ENDIF
C
      NP1 = N + 1
      KRANKE = MIN(ME,N)
      N1 = 2*KRANKE + 1
      N2 = N1 + N
C
C     Set nominal values.
C
C     The nominal column scaling used in the code is
C     the identity scaling.
C
      CALL DCOPY (N, 1.D0, 0, WS(N1), 1)
C
C     No covariance matrix is nominally computed.
C
      COV = .FALSE.
C
C     Process option vector.
C     Define bound for number of options to change.
C
      NOPT = 1000
      NTIMES = 0
C
C     Define bound for positive values of LINK.
C
      NLINK = 100000
      LAST = 1
      LINK = PRGOPT(1)
      IF (LINK.EQ.0 .OR. LINK.GT.NLINK) THEN
         CALL XERMSG ('SLATEC', 'DLSEI',
     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
         RETURN
      ENDIF
C
  100 IF (LINK.GT.1) THEN
         NTIMES = NTIMES + 1
         IF (NTIMES.GT.NOPT) THEN
            CALL XERMSG ('SLATEC', 'DLSEI',
     +         'THE LINKS IN THE OPTION VECTOR ARE CYCLING.', 2, 1)
            RETURN
         ENDIF
C
         KEY = PRGOPT(LAST+1)
         IF (KEY.EQ.1) THEN
            COV = PRGOPT(LAST+2) .NE. 0.D0
         ELSEIF (KEY.EQ.2 .AND. PRGOPT(LAST+2).NE.0.D0) THEN
            DO 110 J = 1,N
               T = DNRM2(M,W(1,J),1)
               IF (T.NE.0.D0) T = 1.D0/T
               WS(J+N1-1) = T
  110       CONTINUE
         ELSEIF (KEY.EQ.3) THEN
            CALL DCOPY (N, PRGOPT(LAST+2), 1, WS(N1), 1)
         ELSEIF (KEY.EQ.4) THEN
            TAU = MAX(DRELPR,PRGOPT(LAST+2))
         ENDIF
C
         NEXT = PRGOPT(LINK)
         IF (NEXT.LE.0 .OR. NEXT.GT.NLINK) THEN
         CALL XERMSG ('SLATEC', 'DLSEI',
     +      'THE OPTION VECTOR IS UNDEFINED', 2, 1)
            RETURN
         ENDIF
C
         LAST = LINK
         LINK = NEXT
         GO TO 100
      ENDIF
C
      DO 120 J = 1,N
         CALL DSCAL (M, WS(N1+J-1), W(1,J), 1)
  120 CONTINUE
C
      IF (COV .AND. MDW.LT.N) THEN
         CALL XERMSG ('SLATEC', 'DLSEI',
     +      'MDW .LT. N WHEN COV MATRIX NEEDED, IS AN ERROR', 2, 1)
         RETURN
      ENDIF
C
C     Problem definition and option vector OK.
C
      MODE = 0
C
C     Compute norm of equality constraint matrix and right side.
C
      ENORM = 0.D0
      DO 130 J = 1,N
         ENORM = MAX(ENORM,DASUM(ME,W(1,J),1))
  130 CONTINUE
C
      FNORM = DASUM(ME,W(1,NP1),1)
      SNMAX = 0.D0
      RNMAX = 0.D0
      DO 150 I = 1,KRANKE
C
C        Compute maximum ratio of vector lengths. Partition is at
C        column I.
C
         DO 140 K = I,ME
            SN = DDOT(N-I+1,W(K,I),MDW,W(K,I),MDW)
            RN = DDOT(I-1,W(K,1),MDW,W(K,1),MDW)
            IF (RN.EQ.0.D0 .AND. SN.GT.SNMAX) THEN
               SNMAX = SN
               IMAX = K
            ELSEIF (K.EQ.I .OR. SN*RNMAX.GT.RN*SNMAX) THEN
               SNMAX = SN
               RNMAX = RN
               IMAX = K
            ENDIF
  140    CONTINUE
C
C        Interchange rows if necessary.
C
         IF (I.NE.IMAX) CALL DSWAP (NP1, W(I,1), MDW, W(IMAX,1), MDW)
         IF (SNMAX.GT.RNMAX*TAU**2) THEN
C
C        Eliminate elements I+1,...,N in row I.
C
            CALL DH12 (1, I, I+1, N, W(I,1), MDW, WS(I), W(I+1,1), MDW,
     +                1, M-I)
         ELSE
            KRANKE = I - 1
            GO TO 160
         ENDIF
  150 CONTINUE
C
C     Save diagonal terms of lower trapezoidal matrix.
C
  160 CALL DCOPY (KRANKE, W, MDW+1, WS(KRANKE+1), 1)
C
C     Use Householder transformation from left to achieve
C     KRANKE by KRANKE upper triangular form.
C
      IF (KRANKE.LT.ME) THEN
         DO 170 K = KRANKE,1,-1
C
C           Apply transformation to matrix cols. 1,...,K-1.
C
            CALL DH12 (1, K, KRANKE+1, ME, W(1,K), 1, UP, W, 1, MDW,
     *         K-1)
C
C           Apply to rt side vector.
C
            CALL DH12 (2, K, KRANKE+1, ME, W(1,K), 1, UP, W(1,NP1), 1,
     +         1, 1)
  170    CONTINUE
      ENDIF
C
C     Solve for variables 1,...,KRANKE in new coordinates.
C
      CALL DCOPY (KRANKE, W(1, NP1), 1, X, 1)
      DO 180 I = 1,KRANKE
         X(I) = (X(I)-DDOT(I-1,W(I,1),MDW,X,1))/W(I,I)
  180 CONTINUE
C
C     Compute residuals for reduced problem.
C
      MEP1 = ME + 1
      RNORML = 0.D0
      DO 190 I = MEP1,M
         W(I,NP1) = W(I,NP1) - DDOT(KRANKE,W(I,1),MDW,X,1)
         SN = DDOT(KRANKE,W(I,1),MDW,W(I,1),MDW)
         RN = DDOT(N-KRANKE,W(I,KRANKE+1),MDW,W(I,KRANKE+1),MDW)
         IF (RN.LE.SN*TAU**2 .AND. KRANKE.LT.N)
     *      CALL DCOPY (N-KRANKE, 0.D0, 0, W(I,KRANKE+1), MDW)
  190 CONTINUE
C
C     Compute equality constraint equations residual length.
C
      RNORME = DNRM2(ME-KRANKE,W(KRANKE+1,NP1),1)
C
C     Move reduced problem data upward if KRANKE.LT.ME.
C
      IF (KRANKE.LT.ME) THEN
         DO 200 J = 1,NP1
            CALL DCOPY (M-ME, W(ME+1,J), 1, W(KRANKE+1,J), 1)
  200    CONTINUE
      ENDIF
C
C     Compute solution of reduced problem.
C
      CALL DLSI(W(KRANKE+1, KRANKE+1), MDW, MA, MG, N-KRANKE, PRGOPT,
     +         X(KRANKE+1), RNORML, MODE, WS(N2), IP(2))
C
C     Test for consistency of equality constraints.
C
      IF (ME.GT.0) THEN
         MDEQC = 0
         XNRME = DASUM(KRANKE,W(1,NP1),1)
         IF (RNORME.GT.TAU*(ENORM*XNRME+FNORM)) MDEQC = 1
         MODE = MODE + MDEQC
C
C        Check if solution to equality constraints satisfies inequality
C        constraints when there are no degrees of freedom left.
C
         IF (KRANKE.EQ.N .AND. MG.GT.0) THEN
            XNORM = DASUM(N,X,1)
            MAPKE1 = MA + KRANKE + 1
            MEND = MA + KRANKE + MG
            DO 210 I = MAPKE1,MEND
               SIZE = DASUM(N,W(I,1),MDW)*XNORM + ABS(W(I,NP1))
               IF (W(I,NP1).GT.TAU*SIZE) THEN
                  MODE = MODE + 2
                  GO TO 290
               ENDIF
  210       CONTINUE
         ENDIF
      ENDIF
C
C     Replace diagonal terms of lower trapezoidal matrix.
C
      IF (KRANKE.GT.0) THEN
         CALL DCOPY (KRANKE, WS(KRANKE+1), 1, W, MDW+1)
C
C        Reapply transformation to put solution in original coordinates.
C
         DO 220 I = KRANKE,1,-1
            CALL DH12 (2, I, I+1, N, W(I,1), MDW, WS(I), X, 1, 1, 1)
  220    CONTINUE
C
C        Compute covariance matrix of equality constrained problem.
C
         IF (COV) THEN
            DO 270 J = MIN(KRANKE,N-1),1,-1
               RB = WS(J)*W(J,J)
               IF (RB.NE.0.D0) RB = 1.D0/RB
               JP1 = J + 1
               DO 230 I = JP1,N
                  W(I,J) = RB*DDOT(N-J,W(I,JP1),MDW,W(J,JP1),MDW)
  230          CONTINUE
C
               GAM = 0.5D0*RB*DDOT(N-J,W(JP1,J),1,W(J,JP1),MDW)
               CALL DAXPY (N-J, GAM, W(J,JP1), MDW, W(JP1,J), 1)
               DO 250 I = JP1,N
                  DO 240 K = I,N
                     W(I,K) = W(I,K) + W(J,I)*W(K,J) + W(I,J)*W(J,K)
                     W(K,I) = W(I,K)
  240             CONTINUE
  250          CONTINUE
               UJ = WS(J)
               VJ = GAM*UJ
               W(J,J) = UJ*VJ + UJ*VJ
               DO 260 I = JP1,N
                  W(J,I) = UJ*W(I,J) + VJ*W(J,I)
  260          CONTINUE
               CALL DCOPY (N-J, W(J, JP1), MDW, W(JP1,J), 1)
  270       CONTINUE
         ENDIF
      ENDIF
C
C     Apply the scaling to the covariance matrix.
C
      IF (COV) THEN
         DO 280 I = 1,N
            CALL DSCAL (N, WS(I+N1-1), W(I,1), MDW)
            CALL DSCAL (N, WS(I+N1-1), W(1,I), 1)
  280    CONTINUE
      ENDIF
C
C     Rescale solution vector.
C
  290 IF (MODE.LE.1) THEN
         DO 300 J = 1,N
            X(J) = X(J)*WS(N1+J-1)
  300    CONTINUE
      ENDIF
C
      IP(1) = KRANKE
      IP(3) = IP(3) + 2*KRANKE + N
      RETURN
      END
