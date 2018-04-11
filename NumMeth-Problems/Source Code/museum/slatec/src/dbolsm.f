      SUBROUTINE DBOLSM (W, MDW, MINPUT, NCOLS, BL, BU, IND, IOPT, X,
     +   RNORM, MODE, RW, WW, SCL, IBASIS, IBB)
C
C     PURPOSE
C     -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE BOUNDED
C     LEAST SQUARES PROBLEM.  THE PROBLEM SOLVED HERE IS:
C
C     SOLVE E*X =  F  (LEAST SQUARES SENSE)
C     WITH BOUNDS ON SELECTED X VALUES.
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THE SUBPROGRAM NAME TO DBOLSM AND THE STRINGS
C     /SAXPY/ TO /DAXPY/, /SCOPY/ TO /DCOPY/,
C     /SDOT/ TO /DDOT/, /SNRM2/ TO /DNRM2/,
C     /SROT/ TO /DROT/, /SROTG/ TO /DROTG/, /R1MACH/ TO /D1MACH/,
C     /SVOUT/ TO /DVOUT/, /SMOUT/ TO /DMOUT/,
C     /SSWAP/ TO /DSWAP/, /E0/ TO /D0/,
C     /REAL            / TO /DOUBLE PRECISION/.
C++
C
      DOUBLE PRECISION W(MDW,*),BL(*),BU(*)
      DOUBLE PRECISION X(*),RW(*),WW(*),SCL(*)
      DOUBLE PRECISION ALPHA,BETA,BOU,COLABV,COLBLO
      DOUBLE PRECISION CL1,CL2,CL3,ONE,BIG
      DOUBLE PRECISION FAC,RNORM,SC,SS,T,TOLIND,WT
      DOUBLE PRECISION TWO,T1,T2,WBIG,WLARGE,WMAG,XNEW
      DOUBLE PRECISION ZERO,DDOT,DNRM2
      DOUBLE PRECISION D1MACH,TOLSZE
      INTEGER IBASIS(*),IBB(*),IND(*),IOPT(*)
      LOGICAL FOUND,CONSTR
      CHARACTER*8 XERN1, XERN2
      CHARACTER*16 XERN3, XERN4
C
      PARAMETER (ZERO=0.0D0, ONE=1.0D0, TWO=2.0D0)
C
      INEXT(IDUM) = MIN(IDUM+1,MROWS)
C***FIRST EXECUTABLE STATEMENT  DBOLSM
C
C     Verify that the problem dimensions are defined properly.
C
      IF (MINPUT.LE.0) THEN
          WRITE (XERN1, '(I8)') MINPUT
          CALL XERMSG ('SLATEC', 'DBOLSM', 'THE NUMBER OF ROWS = ' //
     *       XERN1 // ' MUST BE POSITIVE.', 31, 1)
          MODE = -31
          RETURN
      ENDIF
C
      IF (NCOLS.LE.0) THEN
          WRITE (XERN1, '(I8)') NCOLS
          CALL XERMSG ('SLATEC', 'DBOLSM', 'THE NUMBER OF COLUMNS = ' //
     *       XERN1 // ' MUST BE POSITIVE.', 32, 1)
          MODE = -32
          RETURN
      ENDIF
C
      IF (MDW.LT.MINPUT) THEN
          WRITE (XERN1, '(I8)') MDW
          WRITE (XERN2, '(I8)') MINPUT
          CALL XERMSG ('SLATEC', 'DBOLSM',
     *       'THE ROW DIMENSION OF W(,) = ' // XERN1 //
     *       ' MUST BE .GE. THE NUMBER OF ROWS = ' // XERN2, 33, 1)
          MODE = -33
          RETURN
      ENDIF
C
C     Verify that bound information is correct.
C
      DO 10 J = 1,NCOLS
          IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
              WRITE (XERN1, '(I8)') J
              WRITE (XERN2, '(I8)') IND(J)
              CALL XERMSG ('SLATEC', 'DBOLSM', 'FOR J = ' // XERN1 //
     *           ' THE CONSTRAINT INDICATOR MUST BE 1-4', 34, 1)
              MODE = -34
              RETURN
          ENDIF
   10 CONTINUE
C
      DO 20 J = 1,NCOLS
          IF (IND(J).EQ.3) THEN
              IF (BU(J).LT.BL(J)) THEN
                  WRITE (XERN1, '(I8)') J
                  WRITE (XERN3, '(1PD15.6)') BL(J)
                  WRITE (XERN4, '(1PD15.6)') BU(J)
                  CALL XERMSG ('SLATEC', 'DBOLSM', 'FOR J = ' // XERN1
     *               // ' THE LOWER BOUND = ' // XERN3 //
     *               ' IS .GT. THE UPPER BOUND = ' // XERN4, 35, 1)
                  MODE = -35
                  RETURN
              ENDIF
          ENDIF
   20 CONTINUE
C
C     Check that permutation and polarity arrays have been set.
C
      DO 30 J = 1,NCOLS
          IF (IBASIS(J).LT.1 .OR. IBASIS(J).GT.NCOLS) THEN
              WRITE (XERN1, '(I8)') IBASIS(J)
              WRITE (XERN2, '(I8)') NCOLS
              CALL XERMSG ('SLATEC', 'DBOLSM',
     *           'THE INPUT ORDER OF COLUMNS = ' // XERN1 //
     *           ' IS NOT BETWEEN 1 AND NCOLS = ' // XERN2, 36, 1)
              MODE = -36
              RETURN
          ENDIF
C
          IF (IBB(J).LE.0) THEN
              WRITE (XERN1, '(I8)') J
              WRITE (XERN2, '(I8)') IBB(J)
              CALL XERMSG ('SLATEC', 'DBOLSM',
     *           'THE BOUND POLARITY FLAG IN COMPONENT J = ' // XERN1 //
     *           ' MUST BE POSITIVE.$$NOW = ' // XERN2, 37, 1)
              MODE = -37
              RETURN
          ENDIF
   30 CONTINUE
C
C     Process the option array.
C
      FAC = 0.75D0
      TOLIND = SQRT(D1MACH(4))
      TOLSZE = SQRT(D1MACH(4))
      ITMAX = 5*MAX(MINPUT,NCOLS)
      WT = ONE
      MVAL = 0
      IPRINT = 0
C
C     Changes to some parameters can occur through the option array,
C     IOPT(*).  Process this array looking carefully for input data
C     errors.
C
      LP = 0
      LDS = 0
C
C     Test for no more options.
C
  590 LP = LP + LDS
      IP = IOPT(LP+1)
      JP = ABS(IP)
      IF (IP.EQ.99) THEN
          GO TO 470
      ELSE IF (JP.EQ.99) THEN
          LDS = 1
      ELSE IF (JP.EQ.1) THEN
C
C         Move the IOPT(*) processing pointer.
C
          IF (IP.GT.0) THEN
              LP = IOPT(LP+2) - 1
              LDS = 0
          ELSE
              LDS = 2
          ENDIF
      ELSE IF (JP.EQ.2) THEN
C
C         Change tolerance for rank determination.
C
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  WRITE (XERN1, '(I8)') IOFF
                  WRITE (XERN2, '(I8)') NCOLS
                  CALL XERMSG ('SLATEC', 'DBOLSM', 'THE OFFSET = ' //
     *               XERN1 // ' BEYOND POSITION NCOLS = ' // XERN2 //
     *               ' MUST BE POSITIVE FOR OPTION NUMBER 2.', 24, 1)
                  MODE = -24
                  RETURN
              ENDIF
C
              TOLIND = X(NCOLS+IOFF)
              IF (TOLIND.LT.D1MACH(4)) THEN
                  WRITE (XERN3, '(1PD15.6)') TOLIND
                  WRITE (XERN4, '(1PD15.6)') D1MACH(4)
                  CALL XERMSG ('SLATEC', 'DBOLSM',
     *               'THE TOLERANCE FOR RANK DETERMINATION = ' // XERN3
     *               // ' IS LESS THAN MACHINE PRECISION = ' // XERN4,
     *               25, 0)
                  MODE = -25
              ENDIF
          ENDIF
          LDS = 2
      ELSE IF (JP.EQ.3) THEN
C
C         Change blowup factor for allowing variables to become
C         inactive.
C
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  WRITE (XERN1, '(I8)') IOFF
                  WRITE (XERN2, '(I8)') NCOLS
                  CALL XERMSG ('SLATEC', 'DBOLSM', 'THE OFFSET = ' //
     *               XERN1 // ' BEYOND POSITION NCOLS = ' // XERN2 //
     *               ' MUST BE POSITIVE FOR OPTION NUMBER 3.', 26, 1)
                  MODE = -26
                  RETURN
              ENDIF
C
              TOLSZE = X(NCOLS+IOFF)
              IF (TOLSZE.LE.ZERO) THEN
                  WRITE (XERN3, '(1PD15.6)') TOLSZE
                  CALL XERMSG ('SLATEC', 'DBOLSM', 'THE RECIPROCAL ' //
     *               'OF THE BLOW-UP FACTOR FOR REJECTING VARIABLES ' //
     *               'MUST BE POSITIVE.$$NOW = ' // XERN3, 27, 1)
                  MODE = -27
                  RETURN
              ENDIF
          ENDIF
          LDS = 2
      ELSE IF (JP.EQ.4) THEN
C
C         Change the maximum number of iterations allowed.
C
          IF (IP.GT.0) THEN
              ITMAX = IOPT(LP+2)
              IF (ITMAX.LE.0) THEN
                  WRITE (XERN1, '(I8)') ITMAX
                  CALL XERMSG ('SLATEC', 'DBOLSM',
     *               'THE MAXIMUM NUMBER OF ITERATIONS = ' // XERN1 //
     *               ' MUST BE POSITIVE.', 28, 1)
                  MODE = -28
                  RETURN
              ENDIF
          ENDIF
          LDS = 2
      ELSE IF (JP.EQ.5) THEN
C
C         Change the factor for pretriangularizing the data matrix.
C
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              IF (IOFF.LE.0) THEN
                  WRITE (XERN1, '(I8)') IOFF
                  WRITE (XERN2, '(I8)') NCOLS
                  CALL XERMSG ('SLATEC', 'DBOLSM', 'THE OFFSET = ' //
     *               XERN1 // ' BEYOND POSITION NCOLS = ' // XERN2 //
     *               ' MUST BE POSITIVE FOR OPTION NUMBER 5.', 29, 1)
                  MODE = -29
                  RETURN
              ENDIF
C
              FAC = X(NCOLS+IOFF)
              IF (FAC.LT.ZERO) THEN
                  WRITE (XERN3, '(1PD15.6)') FAC
                  CALL XERMSG ('SLATEC', 'DBOLSM',
     *               'THE FACTOR (NCOLS/MINPUT) WHERE PRE-' //
     *               'TRIANGULARIZING IS PERFORMED MUST BE NON-' //
     *               'NEGATIVE.$$NOW = ' // XERN3, 30, 0)
                  MODE = -30
                  RETURN
              ENDIF
          ENDIF
          LDS = 2
      ELSE IF (JP.EQ.6) THEN
C
C         Change the weighting factor (from 1.0) to apply to components
C         numbered .gt. MVAL (initially set to 1.)  This trick is needed
C         for applications of this subprogram to the heavily weighted
C         least squares problem that come from equality constraints.
C
          IF (IP.GT.0) THEN
              IOFF = IOPT(LP+2)
              MVAL = IOPT(LP+3)
              WT = X(NCOLS+IOFF)
          ENDIF
C
          IF (MVAL.LT.0 .OR. MVAL.GT.MINPUT .OR. WT.LE.ZERO) THEN
              WRITE (XERN1, '(I8)') MVAL
              WRITE (XERN2, '(I8)') MINPUT
              WRITE (XERN3, '(1PD15.6)') WT
              CALL XERMSG ('SLATEC', 'DBOLSM',
     *           'THE ROW SEPARATOR TO APPLY WEIGHTING (' // XERN1 //
     *           ') MUST LIE BETWEEN 0 AND MINPUT = ' // XERN2 //
     *           '.$$WEIGHT = ' // XERN3 // ' MUST BE POSITIVE.', 38, 0)
              MODE = -38
              RETURN
          ENDIF
          LDS = 3
      ELSE IF (JP.EQ.7) THEN
C
C         Turn on debug output.
C
          IF (IP.GT.0) IPRINT = 1
          LDS = 2
      ELSE
          WRITE (XERN1, '(I8)') IP
          CALL XERMSG ('SLATEC', 'DBOLSM', 'THE OPTION NUMBER = ' //
     *       XERN1 // ' IS NOT DEFINED.', 23, 1)
          MODE = -23
          RETURN
      ENDIF
      GO TO 590
C
C     Pretriangularize rectangular arrays of certain sizes for
C     increased efficiency.
C
  470 IF (FAC*MINPUT.GT.NCOLS) THEN
          DO 490 J = 1,NCOLS+1
              DO 480 I = MINPUT,J+MVAL+1,-1
                  CALL DROTG(W(I-1,J),W(I,J),SC,SS)
                  W(I,J) = ZERO
                  CALL DROT(NCOLS-J+1,W(I-1,J+1),MDW,W(I,J+1),MDW,SC,SS)
  480         CONTINUE
  490     CONTINUE
          MROWS = NCOLS + MVAL + 1
      ELSE
          MROWS = MINPUT
      ENDIF
C
C     Set the X(*) array to zero so all components are defined.
C
      CALL DCOPY(NCOLS,ZERO,0,X,1)
C
C     The arrays IBASIS(*) and IBB(*) are initialized by the calling
C     program and the column scaling is defined in the calling program.
C     'BIG' is plus infinity on this machine.
C
      BIG = D1MACH(2)
      DO 550 J = 1,NCOLS
          IF (IND(J).EQ.1) THEN
              BU(J) = BIG
          ELSE IF (IND(J).EQ.2) THEN
              BL(J) = -BIG
          ELSE IF (IND(J).EQ.4) THEN
              BL(J) = -BIG
              BU(J) = BIG
          ENDIF
  550 CONTINUE
C
      DO 570 J = 1,NCOLS
          IF ((BL(J).LE.ZERO.AND.ZERO.LE.BU(J).AND.ABS(BU(J)).LT.
     *        ABS(BL(J))) .OR. BU(J).LT.ZERO) THEN
              T = BU(J)
              BU(J) = -BL(J)
              BL(J) = -T
              SCL(J) = -SCL(J)
              DO 560 I = 1,MROWS
                  W(I,J) = -W(I,J)
  560         CONTINUE
          ENDIF
C
C         Indices in set T(=TIGHT) are denoted by negative values
C         of IBASIS(*).
C
          IF (BL(J).GE.ZERO) THEN
              IBASIS(J) = -IBASIS(J)
              T = -BL(J)
              BU(J) = BU(J) + T
              CALL DAXPY(MROWS,T,W(1,J),1,W(1,NCOLS+1),1)
          ENDIF
  570 CONTINUE
C
      NSETB = 0
      ITER = 0
C
      IF (IPRINT.GT.0) THEN
          CALL DMOUT(MROWS,NCOLS+1,MDW,W,'('' PRETRI. INPUT MATRIX'')',
     *               -4)
          CALL DVOUT(NCOLS,BL,'('' LOWER BOUNDS'')',-4)
          CALL DVOUT(NCOLS,BU,'('' UPPER BOUNDS'')',-4)
      ENDIF
C
  580 ITER = ITER + 1
      IF (ITER.GT.ITMAX) THEN
         WRITE (XERN1, '(I8)') ITMAX
         CALL XERMSG ('SLATEC', 'DBOLSM', 'MORE THAN ITMAX = ' // XERN1
     *      // ' ITERATIONS SOLVING BOUNDED LEAST SQUARES PROBLEM.',
     *      22, 1)
         MODE = -22
C
C        Rescale and translate variables.
C
         IGOPR = 1
         GO TO 130
      ENDIF
C
C     Find a variable to become non-active.
C                                                 T
C     Compute (negative) of gradient vector, W = E *(F-E*X).
C
      CALL DCOPY(NCOLS,ZERO,0,WW,1)
      DO 200 J = NSETB+1,NCOLS
          JCOL = ABS(IBASIS(J))
          WW(J) = DDOT(MROWS-NSETB,W(INEXT(NSETB),J),1,
     *            W(INEXT(NSETB),NCOLS+1),1)*ABS(SCL(JCOL))
  200 CONTINUE
C
      IF (IPRINT.GT.0) THEN
          CALL DVOUT(NCOLS,WW,'('' GRADIENT VALUES'')',-4)
          CALL IVOUT(NCOLS,IBASIS,'('' INTERNAL VARIABLE ORDER'')',-4)
          CALL IVOUT(NCOLS,IBB,'('' BOUND POLARITY'')',-4)
      ENDIF
C
C     If active set = number of total rows, quit.
C
  210 IF (NSETB.EQ.MROWS) THEN
          FOUND = .FALSE.
          GO TO 120
      ENDIF
C
C     Choose an extremal component of gradient vector for a candidate
C     to become non-active.
C
      WLARGE = -BIG
      WMAG = -BIG
      DO 220 J = NSETB+1,NCOLS
          T = WW(J)
          IF (T.EQ.BIG) GO TO 220
          ITEMP = IBASIS(J)
          JCOL = ABS(ITEMP)
          T1 = DNRM2(MVAL-NSETB,W(INEXT(NSETB),J),1)
          IF (ITEMP.LT.0) THEN
              IF (MOD(IBB(JCOL),2).EQ.0) T = -T
              IF (T.LT.ZERO) GO TO 220
              IF (MVAL.GT.NSETB) T = T1
              IF (T.GT.WLARGE) THEN
                  WLARGE = T
                  JLARGE = J
              ENDIF
          ELSE
              IF (MVAL.GT.NSETB) T = T1
              IF (ABS(T).GT.WMAG) THEN
                  WMAG = ABS(T)
                  JMAG = J
              ENDIF
          ENDIF
  220 CONTINUE
C
C     Choose magnitude of largest component of gradient for candidate.
C
      JBIG = 0
      WBIG = ZERO
      IF (WLARGE.GT.ZERO) THEN
          JBIG = JLARGE
          WBIG = WLARGE
      ENDIF
C
      IF (WMAG.GE.WBIG) THEN
          JBIG = JMAG
          WBIG = WMAG
      ENDIF
C
      IF (JBIG.EQ.0) THEN
          FOUND = .FALSE.
          IF (IPRINT.GT.0) THEN
              CALL IVOUT(0,I,'('' FOUND NO VARIABLE TO ENTER'')',-4)
          ENDIF
          GO TO 120
      ENDIF
C
C     See if the incoming column is sufficiently independent.  This
C     test is made before an elimination is performed.
C
      IF (IPRINT.GT.0)
     *    CALL IVOUT(1,JBIG,'('' TRY TO BRING IN THIS COL.'')',-4)
C
      IF (MVAL.LE.NSETB) THEN
          CL1 = DNRM2(MVAL,W(1,JBIG),1)
          CL2 = ABS(WT)*DNRM2(NSETB-MVAL,W(INEXT(MVAL),JBIG),1)
          CL3 = ABS(WT)*DNRM2(MROWS-NSETB,W(INEXT(NSETB),JBIG),1)
          CALL DROTG(CL1,CL2,SC,SS)
          COLABV = ABS(CL1)
          COLBLO = CL3
      ELSE
          CL1 = DNRM2(NSETB,W(1,JBIG),1)
          CL2 = DNRM2(MVAL-NSETB,W(INEXT(NSETB),JBIG),1)
          CL3 = ABS(WT)*DNRM2(MROWS-MVAL,W(INEXT(MVAL),JBIG),1)
          COLABV = CL1
          CALL DROTG(CL2,CL3,SC,SS)
          COLBLO = ABS(CL2)
      ENDIF
C
      IF (COLBLO.LE.TOLIND*COLABV) THEN
          WW(JBIG) = BIG
          IF (IPRINT.GT.0)
     *        CALL IVOUT(0,I,'('' VARIABLE IS DEPENDENT, NOT USED.'')',
     *           -4)
          GO TO 210
      ENDIF
C
C     Swap matrix columns NSETB+1 and JBIG, plus pointer information,
C     and gradient values.
C
      NSETB = NSETB + 1
      IF (NSETB.NE.JBIG) THEN
          CALL DSWAP(MROWS,W(1,NSETB),1,W(1,JBIG),1)
          CALL DSWAP(1,WW(NSETB),1,WW(JBIG),1)
          ITEMP = IBASIS(NSETB)
          IBASIS(NSETB) = IBASIS(JBIG)
          IBASIS(JBIG) = ITEMP
      ENDIF
C
C     Eliminate entries below the pivot line in column NSETB.
C
      IF (MROWS.GT.NSETB) THEN
          DO 230 I = MROWS,NSETB+1,-1
              IF (I.EQ.MVAL+1) GO TO 230
              CALL DROTG(W(I-1,NSETB),W(I,NSETB),SC,SS)
              W(I,NSETB) = ZERO
              CALL DROT(NCOLS-NSETB+1,W(I-1,NSETB+1),MDW,W(I,NSETB+1),
     *                  MDW,SC,SS)
  230     CONTINUE
C
          IF (MVAL.GE.NSETB .AND. MVAL.LT.MROWS) THEN
              CALL DROTG(W(NSETB,NSETB),W(MVAL+1,NSETB),SC,SS)
              W(MVAL+1,NSETB) = ZERO
              CALL DROT(NCOLS-NSETB+1,W(NSETB,NSETB+1),MDW,
     *                  W(MVAL+1,NSETB+1),MDW,SC,SS)
          ENDIF
      ENDIF
C
      IF (W(NSETB,NSETB).EQ.ZERO) THEN
          WW(NSETB) = BIG
          NSETB = NSETB - 1
          IF (IPRINT.GT.0) THEN
              CALL IVOUT(0,I,'('' PIVOT IS ZERO, NOT USED.'')',-4)
          ENDIF
          GO TO 210
      ENDIF
C
C     Check that new variable is moving in the right direction.
C
      ITEMP = IBASIS(NSETB)
      JCOL = ABS(ITEMP)
      XNEW = (W(NSETB,NCOLS+1)/W(NSETB,NSETB))/ABS(SCL(JCOL))
      IF (ITEMP.LT.0) THEN
C
C         IF(WW(NSETB).GE.ZERO.AND.XNEW.LE.ZERO) exit(quit)
C         IF(WW(NSETB).LE.ZERO.AND.XNEW.GE.ZERO) exit(quit)
C
          IF ((WW(NSETB).GE.ZERO.AND.XNEW.LE.ZERO) .OR.
     *        (WW(NSETB).LE.ZERO.AND.XNEW.GE.ZERO)) GO TO 240
      ENDIF
      FOUND = .TRUE.
      GO TO 120
C
  240 WW(NSETB) = BIG
      NSETB = NSETB - 1
      IF (IPRINT.GT.0)
     *    CALL IVOUT(0,I,'('' VARIABLE HAS BAD DIRECTION, NOT USED.'')',
     *       -4)
      GO TO 210
C
C     Solve the triangular system.
C
  270 CALL DCOPY(NSETB,W(1,NCOLS+1),1,RW,1)
      DO 280 J = NSETB,1,-1
          RW(J) = RW(J)/W(J,J)
          JCOL = ABS(IBASIS(J))
          T = RW(J)
          IF (MOD(IBB(JCOL),2).EQ.0) RW(J) = -RW(J)
          CALL DAXPY(J-1,-T,W(1,J),1,RW,1)
          RW(J) = RW(J)/ABS(SCL(JCOL))
  280 CONTINUE
C
      IF (IPRINT.GT.0) THEN
          CALL DVOUT(NSETB,RW,'('' SOLN. VALUES'')',-4)
          CALL IVOUT(NSETB,IBASIS,'('' COLS. USED'')',-4)
      ENDIF
C
      IF (LGOPR.EQ.2) THEN
          CALL DCOPY(NSETB,RW,1,X,1)
          DO 450 J = 1,NSETB
              ITEMP = IBASIS(J)
              JCOL = ABS(ITEMP)
              IF (ITEMP.LT.0) THEN
                  BOU = ZERO
              ELSE
                  BOU = BL(JCOL)
              ENDIF
C
              IF ((-BOU).NE.BIG) BOU = BOU/ABS(SCL(JCOL))
              IF (X(J).LE.BOU) THEN
                  JDROP1 = J
                  GO TO 340
              ENDIF
C
              BOU = BU(JCOL)
              IF (BOU.NE.BIG) BOU = BOU/ABS(SCL(JCOL))
              IF (X(J).GE.BOU) THEN
                  JDROP2 = J
                  GO TO 340
              ENDIF
  450     CONTINUE
          GO TO 340
      ENDIF
C
C     See if the unconstrained solution (obtained by solving the
C     triangular system) satisfies the problem bounds.
C
      ALPHA = TWO
      BETA = TWO
      X(NSETB) = ZERO
      DO 310 J = 1,NSETB
          ITEMP = IBASIS(J)
          JCOL = ABS(ITEMP)
          T1 = TWO
          T2 = TWO
          IF (ITEMP.LT.0) THEN
              BOU = ZERO
          ELSE
              BOU = BL(JCOL)
          ENDIF
          IF ((-BOU).NE.BIG) BOU = BOU/ABS(SCL(JCOL))
          IF (RW(J).LE.BOU) T1 = (X(J)-BOU)/ (X(J)-RW(J))
          BOU = BU(JCOL)
          IF (BOU.NE.BIG) BOU = BOU/ABS(SCL(JCOL))
          IF (RW(J).GE.BOU) T2 = (BOU-X(J))/ (RW(J)-X(J))
C
C     If not, then compute a step length so that the variables remain
C     feasible.
C
          IF (T1.LT.ALPHA) THEN
              ALPHA = T1
              JDROP1 = J
          ENDIF
C
          IF (T2.LT.BETA) THEN
              BETA = T2
              JDROP2 = J
          ENDIF
  310 CONTINUE
C
      CONSTR = ALPHA .LT. TWO .OR. BETA .LT. TWO
      IF (.NOT.CONSTR) THEN
C
C         Accept the candidate because it satisfies the stated bounds
C         on the variables.
C
          CALL DCOPY(NSETB,RW,1,X,1)
          GO TO 580
      ENDIF
C
C     Take a step that is as large as possible with all variables
C     remaining feasible.
C
      DO 330 J = 1,NSETB
          X(J) = X(J) + MIN(ALPHA,BETA)* (RW(J)-X(J))
  330 CONTINUE
C
      IF (ALPHA.LE.BETA) THEN
          JDROP2 = 0
      ELSE
          JDROP1 = 0
      ENDIF
C
  340 IF (JDROP1+JDROP2.LE.0 .OR. NSETB.LE.0) GO TO 580
  350 JDROP = JDROP1 + JDROP2
      ITEMP = IBASIS(JDROP)
      JCOL = ABS(ITEMP)
      IF (JDROP2.GT.0) THEN
C
C         Variable is at an upper bound.  Subtract multiple of this
C         column from right hand side.
C
          T = BU(JCOL)
          IF (ITEMP.GT.0) THEN
              BU(JCOL) = T - BL(JCOL)
              BL(JCOL) = -T
              ITEMP = -ITEMP
              SCL(JCOL) = -SCL(JCOL)
              DO 360 I = 1,JDROP
                  W(I,JDROP) = -W(I,JDROP)
  360         CONTINUE
          ELSE
              IBB(JCOL) = IBB(JCOL) + 1
              IF (MOD(IBB(JCOL),2).EQ.0) T = -T
          ENDIF
C
C     Variable is at a lower bound.
C
      ELSE
          IF (ITEMP.LT.ZERO) THEN
              T = ZERO
          ELSE
              T = -BL(JCOL)
              BU(JCOL) = BU(JCOL) + T
              ITEMP = -ITEMP
          ENDIF
      ENDIF
C
      CALL DAXPY(JDROP,T,W(1,JDROP),1,W(1,NCOLS+1),1)
C
C     Move certain columns left to achieve upper Hessenberg form.
C
      CALL DCOPY(JDROP,W(1,JDROP),1,RW,1)
      DO 370 J = JDROP+1,NSETB
          IBASIS(J-1) = IBASIS(J)
          X(J-1) = X(J)
          CALL DCOPY(J,W(1,J),1,W(1,J-1),1)
  370 CONTINUE
C
      IBASIS(NSETB) = ITEMP
      W(1,NSETB) = ZERO
      CALL DCOPY(MROWS-JDROP,W(1,NSETB),0,W(JDROP+1,NSETB),1)
      CALL DCOPY(JDROP,RW,1,W(1,NSETB),1)
C
C     Transform the matrix from upper Hessenberg form to upper
C     triangular form.
C
      NSETB = NSETB - 1
      DO 390 I = JDROP,NSETB
C
C         Look for small pivots and avoid mixing weighted and
C         nonweighted rows.
C
          IF (I.EQ.MVAL) THEN
              T = ZERO
              DO 380 J = I,NSETB
                  JCOL = ABS(IBASIS(J))
                  T1 = ABS(W(I,J)*SCL(JCOL))
                  IF (T1.GT.T) THEN
                      JBIG = J
                      T = T1
                  ENDIF
  380         CONTINUE
              GO TO 400
          ENDIF
          CALL DROTG(W(I,I),W(I+1,I),SC,SS)
          W(I+1,I) = ZERO
          CALL DROT(NCOLS-I+1,W(I,I+1),MDW,W(I+1,I+1),MDW,SC,SS)
  390 CONTINUE
      GO TO 430
C
C     The triangularization is completed by giving up the Hessenberg
C     form and triangularizing a rectangular matrix.
C
  400 CALL DSWAP(MROWS,W(1,I),1,W(1,JBIG),1)
      CALL DSWAP(1,WW(I),1,WW(JBIG),1)
      CALL DSWAP(1,X(I),1,X(JBIG),1)
      ITEMP = IBASIS(I)
      IBASIS(I) = IBASIS(JBIG)
      IBASIS(JBIG) = ITEMP
      JBIG = I
      DO 420 J = JBIG,NSETB
          DO 410 I = J+1,MROWS
              CALL DROTG(W(J,J),W(I,J),SC,SS)
              W(I,J) = ZERO
              CALL DROT(NCOLS-J+1,W(J,J+1),MDW,W(I,J+1),MDW,SC,SS)
  410     CONTINUE
  420 CONTINUE
C
C     See if the remaining coefficients are feasible.  They should be
C     because of the way MIN(ALPHA,BETA) was chosen.  Any that are not
C     feasible will be set to their bounds and appropriately translated.
C
  430 JDROP1 = 0
      JDROP2 = 0
      LGOPR = 2
      GO TO 270
C
C     Find a variable to become non-active.
C
  120 IF (FOUND) THEN
          LGOPR = 1
          GO TO 270
      ENDIF
C
C     Rescale and translate variables.
C
      IGOPR = 2
  130 CALL DCOPY(NSETB,X,1,RW,1)
      CALL DCOPY(NCOLS,ZERO,0,X,1)
      DO 140 J = 1,NSETB
          JCOL = ABS(IBASIS(J))
          X(JCOL) = RW(J)*ABS(SCL(JCOL))
  140 CONTINUE
C
      DO 150 J = 1,NCOLS
          IF (MOD(IBB(J),2).EQ.0) X(J) = BU(J) - X(J)
  150 CONTINUE
C
      DO 160 J = 1,NCOLS
          JCOL = IBASIS(J)
          IF (JCOL.LT.0) X(-JCOL) = BL(-JCOL) + X(-JCOL)
  160 CONTINUE
C
      DO 170 J = 1,NCOLS
          IF (SCL(J).LT.ZERO) X(J) = -X(J)
  170 CONTINUE
C
      I = MAX(NSETB,MVAL)
      RNORM = DNRM2(MROWS-I,W(INEXT(I),NCOLS+1),1)
C
      IF (IGOPR.EQ.2) MODE = NSETB
      RETURN
      END
