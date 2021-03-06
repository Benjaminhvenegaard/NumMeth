      SUBROUTINE BINT4 (X, Y, NDATA, IBCL, IBCR, FBCL, FBCR, KNTOPT, T,
     +   BCOEF, N, K, W)
C
      INTEGER I, IBCL, IBCR, IFLAG, ILB, ILEFT, IT, IUB, IW, IWP, J,
     1 JW, K, KNTOPT, N, NDATA, NDM, NP, NWROW
      REAL BCOEF,FBCL,FBCR,T, TOL,TXN,TX1,VNIKX,W,WDTOL,WORK,X, XL,
     1 Y
      REAL R1MACH
      DIMENSION X(*), Y(*), T(*), BCOEF(*), W(5,*), VNIKX(4,4), WORK(15)
C***FIRST EXECUTABLE STATEMENT  BINT4
      WDTOL = R1MACH(4)
      TOL = SQRT(WDTOL)
      IF (NDATA.LT.2) GO TO 200
      NDM = NDATA - 1
      DO 10 I=1,NDM
        IF (X(I).GE.X(I+1)) GO TO 210
   10 CONTINUE
      IF (IBCL.LT.1 .OR. IBCL.GT.2) GO TO 220
      IF (IBCR.LT.1 .OR. IBCR.GT.2) GO TO 230
      IF (KNTOPT.LT.1 .OR. KNTOPT.GT.3) GO TO 240
      K = 4
      N = NDATA + 2
      NP = N + 1
      DO 20 I=1,NDATA
        T(I+3) = X(I)
   20 CONTINUE
      GO TO (30, 50, 90), KNTOPT
C     SET UP KNOT ARRAY WITH MULTIPLICITY 4 AT X(1) AND X(NDATA)
   30 CONTINUE
      DO 40 I=1,3
        T(4-I) = X(1)
        T(NP+I) = X(NDATA)
   40 CONTINUE
      GO TO 110
C     SET UP KNOT ARRAY WITH SYMMETRIC PLACEMENT ABOUT END POINTS
   50 CONTINUE
      IF (NDATA.GT.3) GO TO 70
      XL = (X(NDATA)-X(1))/3.0E0
      DO 60 I=1,3
        T(4-I) = T(5-I) - XL
        T(NP+I) = T(NP+I-1) + XL
   60 CONTINUE
      GO TO 110
   70 CONTINUE
      TX1 = X(1) + X(1)
      TXN = X(NDATA) + X(NDATA)
      DO 80 I=1,3
        T(4-I) = TX1 - X(I+1)
        T(NP+I) = TXN - X(NDATA-I)
   80 CONTINUE
      GO TO 110
C     SET UP KNOT ARRAY LESS THAN X(1) AND GREATER THAN X(NDATA) TO BE
C     SUPPLIED BY USER IN WORK LOCATIONS W(1) THROUGH W(6) WHEN KNTOPT=3
   90 CONTINUE
      DO 100 I=1,3
        T(4-I) = W(4-I,1)
        JW = MAX(1,I-1)
        IW = MOD(I+2,5)+1
        T(NP+I) = W(IW,JW)
        IF (T(4-I).GT.T(5-I)) GO TO 250
        IF (T(NP+I).LT.T(NP+I-1)) GO TO 250
  100 CONTINUE
  110 CONTINUE
C
      DO 130 I=1,5
        DO 120 J=1,N
          W(I,J) = 0.0E0
  120   CONTINUE
  130 CONTINUE
C     SET UP LEFT INTERPOLATION POINT AND LEFT BOUNDARY CONDITION FOR
C     RIGHT LIMITS
      IT = IBCL + 1
      CALL BSPVD(T, K, IT, X(1), K, 4, VNIKX, WORK)
      IW = 0
      IF (ABS(VNIKX(3,1)).LT.TOL) IW = 1
      DO 140 J=1,3
        W(J+1,4-J) = VNIKX(4-J,IT)
        W(J,4-J) = VNIKX(4-J,1)
  140 CONTINUE
      BCOEF(1) = Y(1)
      BCOEF(2) = FBCL
C     SET UP INTERPOLATION EQUATIONS FOR POINTS I=2 TO I=NDATA-1
      ILEFT = 4
      IF (NDM.LT.2) GO TO 170
      DO 160 I=2,NDM
        ILEFT = ILEFT + 1
        CALL BSPVD(T, K, 1, X(I), ILEFT, 4, VNIKX, WORK)
        DO 150 J=1,3
          W(J+1,3+I-J) = VNIKX(4-J,1)
  150   CONTINUE
        BCOEF(I+1) = Y(I)
  160 CONTINUE
C     SET UP RIGHT INTERPOLATION POINT AND RIGHT BOUNDARY CONDITION FOR
C     LEFT LIMITS(ILEFT IS ASSOCIATED WITH T(N)=X(NDATA-1))
  170 CONTINUE
      IT = IBCR + 1
      CALL BSPVD(T, K, IT, X(NDATA), ILEFT, 4, VNIKX, WORK)
      JW = 0
      IF (ABS(VNIKX(2,1)).LT.TOL) JW = 1
      DO 180 J=1,3
        W(J+1,3+NDATA-J) = VNIKX(5-J,IT)
        W(J+2,3+NDATA-J) = VNIKX(5-J,1)
  180 CONTINUE
      BCOEF(N-1) = FBCR
      BCOEF(N) = Y(NDATA)
C     SOLVE SYSTEM OF EQUATIONS
      ILB = 2 - JW
      IUB = 2 - IW
      NWROW = 5
      IWP = IW + 1
      CALL BNFAC(W(IWP,1), NWROW, N, ILB, IUB, IFLAG)
      IF (IFLAG.EQ.2) GO TO 190
      CALL BNSLV(W(IWP,1), NWROW, N, ILB, IUB, BCOEF)
      RETURN
C
C
  190 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4',
     +   'THE SYSTEM OF EQUATIONS IS SINGULAR', 2, 1)
      RETURN
  200 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4', 'NDATA IS LESS THAN 2', 2, 1)
      RETURN
  210 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4',
     +   'X VALUES ARE NOT DISTINCT OR NOT ORDERED', 2, 1)
      RETURN
  220 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4', 'IBCL IS NOT 1 OR 2', 2, 1)
      RETURN
  230 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4', 'IBCR IS NOT 1 OR 2', 2, 1)
      RETURN
  240 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4', 'KNTOPT IS NOT 1, 2, OR 3', 2, 1)
      RETURN
  250 CONTINUE
      CALL XERMSG ('SLATEC', 'BINT4',
     +   'KNOT INPUT THROUGH W ARRAY IS NOT ORDERED PROPERLY', 2, 1)
      RETURN
      END
