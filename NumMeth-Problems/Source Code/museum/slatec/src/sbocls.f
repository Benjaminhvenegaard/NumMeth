      SUBROUTINE SBOCLS (W, MDW, MCON, MROWS, NCOLS, BL, BU, IND, IOPT,
     +   X, RNORMC, RNORM, MODE, RW, IW)
C     REVISED 850604-0900
C     REVISED YYMMDD-HHMM
C
C    PURPOSE
C    -------
C     THIS IS THE MAIN SUBPROGRAM THAT SOLVES THE LEAST SQUARES
C     PROBLEM CONSISTING OF LINEAR CONSTRAINTS
C
C              C*X = Y
C
C     AND LEAST SQUARES EQUATIONS
C
C              E*X = F
C
C     IN THIS FORMULATION THE VECTORS X AND Y ARE BOTH UNKNOWNS.
C     FURTHER, X AND Y MAY BOTH HAVE USER-SPECIFIED BOUNDS ON EACH
C     COMPONENT.  THE USER MUST HAVE DIMENSION STATEMENTS OF THE
C     FORM
C
C     DIMENSION W(MDW,NCOLS+MCON+1), BL(NCOLS+MCON),BU(NCOLS+MCON),
C               X(2*(NCOLS+MCON)+2+NX), RW(6*NCOLS+5*MCON)
C
C     INTEGER IND(NCOLS+MCON), IOPT(16+NI), IW(2*(NCOLS+MCON))
C
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM TO SBOCLS AND THE STRINGS
C     /SDOT/ TO /DDOT/, /SNRM2/ TO /DNRM2/, /SRELPR/ TO /DRELPR/,
C     /R1MACH/ TO /D1MACH/, /E0/ TO /D0/, /SCOPY/ TO /DCOPY/,
C     /SSCAL/ TO /DSCAL/, /SASUM/ TO /DASUM/, /SBOLS/ TO /DBOLS/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      REAL             W(MDW,*),BL(*),BU(*),X(*),RW(*)
      REAL             ANORM, CNORM, ONE, RNORM, RNORMC, SRELPR
      REAL             T, T1, T2, SDOT, SNRM2, WT, ZERO
      REAL             SASUM, R1MACH
C     THIS VARIABLE REMAINS TYPED REAL.
      INTEGER IND(*),IOPT(*),IW(*),JOPT(05)
      LOGICAL CHECKL,FILTER,ACCUM,PRETRI
      CHARACTER*8 XERN1, XERN2
      CHARACTER*16 XERN3, XERN4
      SAVE IGO,ACCUM,CHECKL
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  SBOCLS
      NERR = 0
      MODE = 0
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              WRITE (XERN1, '(I8)') MDW
              CALL XERMSG ('SLATEC', 'SBOCLS', 'MDW = ' // XERN1 //
     *           ' MUST BE POSITIVE.', 53, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          ENDIF
C
C     SEE THAT NUMBER OF CONSTRAINTS IS NONNEGATIVE.
          IF (MCON.LT.0) THEN
              WRITE (XERN1, '(I8)') MCON
              CALL XERMSG ('SLATEC', 'SBOCLS', 'MCON = ' // XERN1 //
     *           ' MUST BE NON-NEGATIVE', 54, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          ENDIF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              WRITE (XERN1, '(I8)') NCOLS
              CALL XERMSG ('SLATEC', 'SBOCLS', 'NCOLS = ' // XERN1 //
     *           ' THE NO. OF VARIABLES, MUST BE POSITIVE.', 55, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          ENDIF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS + MCON
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  WRITE (XERN1, '(I8)') J
                  WRITE (XERN2, '(I8)') IND(J)
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *              'IND(' // XERN1 // ') = ' // XERN2 //
     *              ' MUST BE 1-4.', 56, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS + MCON
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                     WRITE (XERN1, '(I8)') J
                     WRITE (XERN3, '(1PE15.6)') BL(J)
                     WRITE (XERN4, '(1PE15.6)') BU(J)
                     CALL XERMSG ('SLATEC', 'SBOCLS',
     *                  'BOUND BL(' // XERN1 // ') = ' // XERN3 //
     *                  ' IS .GT. BU(' // XERN1 // ') = ' // XERN4,
     *                  57, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  ENDIF
              ENDIF
   20     CONTINUE
C     END PROCEDURE
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.E0
          ONE = 1.E0
          SRELPR = R1MACH(4)
          CHECKL = .FALSE.
          FILTER = .TRUE.
          LENX = 2* (NCOLS+MCON) + 2
          ISCALE = 1
          IGO = 1
          ACCUM = .FALSE.
          PRETRI = .TRUE.
          LOPT = 0
          MOPT = 0
          LP = 0
          LDS = 0
C     DO FOREVER
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS TO CHANGE.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = - (LP+2)
              IF (MOPT.EQ.0) MOPT = - (ABS(LOPT)+7)
              IF (LOPT.LT.0) THEN
                  LBOU = ABS(LOPT)
              ELSE
                  LBOU = LOPT - 15
              ENDIF
C
C     SEND COL. SCALING TO SBOLS().
              IOPT(LBOU) = 4
              IOPT(LBOU+1) = 1
C
C     PASS AN OPTION ARRAY FOR SBOLSM().
              IOPT(LBOU+2) = 5
C
C     LOC. OF OPTION ARRAY FOR SBOLSM( ).
              IOPT(LBOU+3) = 8
C
C     SKIP TO START OF USER-GIVEN OPTION ARRAY FOR SBOLS().
              IOPT(LBOU+4) = 6
              IOPT(LBOU+6) = 99
              IF (LOPT.GT.0) THEN
                  IOPT(LBOU+5) = LOPT - LBOU + 1
              ELSE
                  IOPT(LBOU+4) = -IOPT(LBOU+4)
              ENDIF
              IF (MOPT.LT.0) THEN
                  LBOUM = ABS(MOPT)
              ELSE
                  LBOUM = MOPT - 8
              ENDIF
C
C     CHANGE PRETRIANGULARIZATION FACTOR IN SBOLSM().
              IOPT(LBOUM) = 5
              IOPT(LBOUM+1) = NCOLS + MCON + 1
C
C     PASS WEIGHT TO SBOLSM() FOR RANK TEST.
              IOPT(LBOUM+2) = 6
              IOPT(LBOUM+3) = NCOLS + MCON + 2
              IOPT(LBOUM+4) = MCON
C
C     SKIP TO USER-GIVEN OPTION ARRAY FOR SBOLSM( ).
              IOPT(LBOUM+5) = 1
              IOPT(LBOUM+7) = 99
              IF (MOPT.GT.0) THEN
                  IOPT(LBOUM+6) = MOPT - LBOUM + 1
              ELSE
                  IOPT(LBOUM+5) = -IOPT(LBOUM+5)
              ENDIF
C     EXIT FOREVER
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
C     CYCLE FOREVER
              GO TO 50
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG LOCATION, ROW STACKING POINTER
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*) BELOW THE ROWS FOR THE CONSTRAINT MATRIX C.
C       SET IOPT(LOCACC+2)=NO. OF LEAST SQUARES EQUATIONS IN BLOCK.
C              LOOP
C              CALL SBOCLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS. INTO W(*,*), STARTING AT
C                      ROW IOPT(LOCACC+1).
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
                  IOPT(LOCACC+1) = MCON + 1
                  ACCUM = .TRUE.
                  IOPT(LOCACC) = IGO
              ENDIF
              LDS = 4
C     CYCLE FOREVER
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MCON+MAX(MOUT,NCOLS), IF MCON.GT.0 .AND FILTER
C     LMDW.GE.MCON+MOUT, OTHERWISE
C
C     LNDW.GE.NCOLS+MCON+1
C     LLB .GE.NCOLS+MCON
C     LLX .GE.2*(NCOLS+MCON)+2+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.6*NCOLS+5*MCON
C     LLIW.GE.2*(NCOLS+MCON)
C     LIOP.GE. AMOUNT REQD. FOR OPTION ARRAY.
                  LMDW = IOPT(LOCDIM)
                  LNDW = IOPT(LOCDIM+1)
                  LLB = IOPT(LOCDIM+2)
                  LLX = IOPT(LOCDIM+3)
                  LLRW = IOPT(LOCDIM+4)
                  LLIW = IOPT(LOCDIM+5)
                  LIOPT = IOPT(LOCDIM+6)
                  CHECKL = .TRUE.
              ENDIF
              LDS = 8
C     CYCLE FOREVER
              GO TO 30
C
C     OPTION TO MODIFY THE COLUMN SCALING.
          ELSE IF (JP.EQ.3) THEN
              IF (IP.GT.0) THEN
                  ISCALE = IOPT(LP+2)
C
C     SEE THAT ISCALE IS 1 THRU 3.
                  IF (ISCALE.LT.1 .OR. ISCALE.GT.3) THEN
                      WRITE (XERN1, '(I8)') ISCALE
                      CALL XERMSG ('SLATEC', 'SBOCLS',
     *                   'ISCALE OPTION = ' // XERN1 // ' MUST BE 1-3',
     *                   48, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  ENDIF
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION THE USER HAS PROVIDED SCALING.  THE
C     SCALE FACTORS FOR THE COLUMNS BEGIN IN X(NCOLS+IOPT(LP+2)).
          ELSE IF (JP.EQ.4) THEN
              IF (IP.GT.0) THEN
                  ISCALE = 4
                  IF (IOPT(LP+2).LE.0) THEN
                      WRITE (XERN1, '(I8)') IOPT(LP+2)
                      CALL XERMSG ('SLATEC', 'SBOCLS',
     *                   'OFFSET PAST X(NCOLS) (' // XERN1 //
     *           ') FOR USER-PROVIDED COLUMN SCALING MUST BE POSITIVE.',
     *                   49, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 260
                  ENDIF
                  CALL SCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          WRITE (XERN1, '(I8)') J
                          WRITE (XERN3, '(1PE15.6)') RW(J)
                          CALL XERMSG ('SLATEC', 'SBOCLS',
     *                       'EACH PROVIDED COLUMN SCALE FACTOR ' //
     *                       'MUST BE POSITIVE.$$COMPONENT ' // XERN1 //
     *                       ' NOW = ' // XERN3, 50, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 260
                      ENDIF
   40             CONTINUE
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLS().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLSM().
          ELSE IF (JP.EQ.6) THEN
              IF (IP.GT.0) THEN
                  MOPT = IOPT(LP+2)
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS A
C     POINTER VALUE TO SKIP TO NEXT.
          ELSE IF (JP.EQ.7) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2) - 1
                  LDS = 0
              ELSE
                  LDS = 2
              ENDIF
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION AVOIDS THE CONSTRAINT RESOLVING PHASE FOR
C     THE LINEAR CONSTRAINTS C*X=Y.
          ELSE IF (JP.EQ.8) THEN
              FILTER = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION SUPPRESSES PRE-TRIANGULARIZATION OF THE LEAST
C     SQUARES EQUATIONS.
          ELSE IF (JP.EQ.9) THEN
              PRETRI = .NOT. (IP.GT.0)
              LDS = 1
C     CYCLE FOREVER
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              WRITE (XERN1, '(I8)') JP
              CALL XERMSG ('SLATEC', 'SBOCLS', 'OPTION NUMBER = ' //
     *           XERN1 // ' IS NOT DEFINED.', 51, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 260
          ENDIF
C     END FOREVER
C     END PROCEDURE
   50     CONTINUE
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
           IF(FILTER .AND. .NOT.ACCUM) THEN
                MDWL=MCON+MAX(MROWS,NCOLS)
           ELSE
                MDWL=MCON+NCOLS+1
           ENDIF
              IF (LMDW.LT.MDWL) THEN
                  WRITE (XERN1, '(I8)') LMDW
                  WRITE (XERN2, '(I8)') MDWL
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *               'THE ROW DIMENSION OF W(,) = ' // XERN1 //
     *               ' MUST BE .GE. THE NUMBER OF EFFECTIVE ROWS = ' //
     *               XERN2, 41, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LNDW.LT.NCOLS+MCON+1) THEN
                  WRITE (XERN1, '(I8)') LNDW
                  WRITE (XERN2, '(I8)') NCOLS+MCON+1
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *               'THE COLUMN DIMENSION OF W(,) = ' // XERN1 //
     *               ' MUST BE .GE. NCOLS+MCON+1 = ' // XERN2, 42, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LLB.LT.NCOLS+MCON) THEN
                  WRITE (XERN1, '(I8)') LLB
                  WRITE (XERN2, '(I8)') NCOLS+MCON
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *           'THE DIMENSIONS OF THE ARRAYS BS(), BU(), AND IND() = '
     *               // XERN1 // ' MUST BE .GE. NCOLS+MCON = ' // XERN2,
     *               43, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LLX.LT.LENX) THEN
                  WRITE (XERN1, '(I8)') LLX
                  WRITE (XERN2, '(I8)') LENX
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *              'THE DIMENSION OF X() = ' // XERN1 //
     *              ' MUST BE .GE. THE REQD. LENGTH = ' // XERN2, 44, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LLRW.LT.6*NCOLS+5*MCON) THEN
                  WRITE (XERN1, '(I8)') LLRW
                  WRITE (XERN2, '(I8)') 6*NCOLS+5*MCON
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *               'THE DIMENSION OF RW() = ' // XERN1 //
     *               ' MUST BE .GE. 6*NCOLS+5*MCON = ' // XERN2, 45, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LLIW.LT.2*NCOLS+2*MCON) THEN
                  WRITE (XERN1, '(I8)') LLIW
                  WRITE (XERN2, '(I8)') 2*NCOLS+2*MCON
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *               'THE DIMENSION OF IW() = ' // XERN1 //
     *               ' MUST BE .GE. 2*NCOLS+2*MCON = ' // XERN2, 46, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
              IF (LIOPT.LT.LP+17) THEN
                  WRITE (XERN1, '(I8)') LIOPT
                  WRITE (XERN2, '(I8)') LP+17
                  CALL XERMSG ('SLATEC', 'SBOCLS',
     *               'THE DIMENSION OF IOPT() = ' // XERN1 //
     *               ' MUST BE .GE. THE REQD. LEN = ' // XERN2, 47, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 260
              ENDIF
C     END PROCEDURE
          ENDIF
      ENDIF
C
C     OPTIONALLY GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
C     EQUATIONS AND DIRECTIONS FOR PROCESSING THESE EQUATIONS.
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      IF (ACCUM) THEN
          MROWS = IOPT(LOCACC+1) - 1 - MCON
          INROWS = IOPT(LOCACC+2)
          MNEW = MROWS + INROWS
          IF (MNEW.LT.0 .OR. MNEW+MCON.GT.MDW) THEN
              WRITE (XERN1, '(I8)') MNEW
              WRITE (XERN2, '(I8)') MDW-MCON
              CALL XERMSG ('SLATEC', 'SBOCLS', 'NO. OF ROWS = ' //
     *           XERN1 //  ' MUST BE .GE. 0 .AND. .LE. MDW-MCON = ' //
     *           XERN2, 52, 1)
C    (RETURN TO USER PROGRAM UNIT)
              GO TO 260
          ENDIF
      ENDIF
C
C     USE THE SOFTWARE OF SBOLS( ) FOR THE TRIANGULARIZATION OF THE
C     LEAST SQUARES MATRIX.  THIS MAY INVOLVE A SYSTALTIC INTERCHANGE
C     OF PROCESSING POINTERS BETWEEN THE CALLING AND CALLED (SBOLS())
C     PROGRAM UNITS.
      JOPT(01) = 1
      JOPT(02) = 2
      JOPT(04) = MROWS
      JOPT(05) = 99
      IRW = NCOLS + 1
      IIW = 1
      IF (ACCUM .OR. PRETRI) THEN
          CALL SBOLS(W(MCON+1,1),MDW,MOUT,NCOLS,BL,BU,IND,JOPT,X,RNORM,
     *               MODE,RW(IRW),IW(IIW))
      ELSE
          MOUT = MROWS
      ENDIF
      IF (ACCUM) THEN
          ACCUM = IOPT(LOCACC) .EQ. 1
          IOPT(LOCACC+1) = JOPT(03) + MCON
          MROWS = MIN(NCOLS+1,MNEW)
      ENDIF
C     END PROCEDURE
      IF (ACCUM) RETURN
C     DO(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE CONSTRAINED AND BOUNDED LEAST SQUARES PROBLEM)
C
C     MOVE RIGHT HAND SIDE OF LEAST SQUARES EQUATIONS.
      CALL SCOPY(MOUT,W(MCON+1,NCOLS+1),1,W(MCON+1,NCOLS+MCON+1),1)
      IF (MCON.GT.0 .AND. FILTER) THEN
C
C     PROJECT THE LINEAR CONSTRAINTS INTO A REACHABLE SET.
          DO 60 I = 1,MCON
              CALL SCOPY(NCOLS,W(I,1),MDW,W(MCON+1,NCOLS+I),1)
   60     CONTINUE
C
C      PLACE (-)IDENTITY MATRIX AFTER CONSTRAINT DATA.
          DO 70 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL SCOPY(MCON,W(1,J),0,W(1,J),1)
   70     CONTINUE
          W(1,NCOLS+1) = -ONE
          CALL SCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
C
C     OBTAIN A 'FEASIBLE POINT' FOR THE LINEAR CONSTRAINTS.
          JOPT(01) = 99
          IRW = NCOLS + 1
          IIW = 1
          CALL SBOLS(W,MDW,MCON,NCOLS+MCON,BL,BU,IND,JOPT,X,RNORMC,
     *               MODEC,RW(IRW),IW(IIW))
C
C     ENLARGE THE BOUNDS SET, IF REQUIRED, TO INCLUDE POINTS THAT
C     CAN BE REACHED.
          DO 130 J = NCOLS + 1,NCOLS + MCON
              ICASE = IND(J)
              IF (ICASE.LT.4) THEN
                  T = SDOT(NCOLS,W(MCON+1,J),1,X,1)
              ENDIF
              GO TO (80,90,100,110),ICASE
              GO TO 120
C     CASE 1
   80         BL(J) = MIN(T,BL(J))
              GO TO 120
C     CASE 2
   90         BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 3
  100         BL(J) = MIN(T,BL(J))
              BU(J) = MAX(T,BU(J))
              GO TO 120
C     CASE 4
  110         CONTINUE
  120         CONTINUE
  130     CONTINUE
C
C     MOVE CONSTRAINT DATA BACK TO THE ORIGINAL AREA.
          DO 140 J = NCOLS + 1,NCOLS + MCON
              CALL SCOPY(NCOLS,W(MCON+1,J),1,W(J-NCOLS,1),MDW)
  140     CONTINUE
      ENDIF
      IF (MCON.GT.0) THEN
          DO 150 J = NCOLS + 1,NCOLS + MCON
              W(MCON+1,J) = ZERO
              CALL SCOPY(MOUT,W(MCON+1,J),0,W(MCON+1,J),1)
  150     CONTINUE
C
C     PUT IN (-)IDENTITY MATRIX (POSSIBLY) ONCE AGAIN.
          DO 160 J = NCOLS + 1,NCOLS + MCON + 1
              W(1,J) = ZERO
              CALL SCOPY(MCON,W(1,J),0,W(1,J),1)
  160     CONTINUE
          W(1,NCOLS+1) = -ONE
          CALL SCOPY(MCON,W(1,NCOLS+1),0,W(1,NCOLS+1),MDW+1)
      ENDIF
C
C     COMPUTE NOMINAL COLUMN SCALING FOR THE UNWEIGHTED MATRIX.
      CNORM = ZERO
      ANORM = ZERO
      DO 170 J = 1,NCOLS
          T1 = SASUM(MCON,W(1,J),1)
          T2 = SASUM(MOUT,W(MCON+1,1),1)
          T = T1 + T2
          IF (T.EQ.ZERO) T = ONE
          CNORM = MAX(CNORM,T1)
          ANORM = MAX(ANORM,T2)
          X(NCOLS+MCON+J) = ONE/T
  170 CONTINUE
      GO TO (180,190,210,220),ISCALE
      GO TO 230
C     CASE 1
  180 CONTINUE
      GO TO 230
C     CASE 2
C
C     SCALE COLS. (BEFORE WEIGHTING) TO HAVE LENGTH ONE.
  190 DO 200 J = 1,NCOLS
          T = SNRM2(MCON+MOUT,W(1,J),1)
          IF (T.EQ.ZERO) T = ONE
          X(NCOLS+MCON+J) = ONE/T
  200 CONTINUE
      GO TO 230
C     CASE 3
C
C     SUPPRESS SCALING (USE UNIT MATRIX).
  210 X(NCOLS+MCON+1) = ONE
      CALL SCOPY(NCOLS,X(NCOLS+MCON+1),0,X(NCOLS+MCON+1),1)
      GO TO 230
C     CASE 4
C
C     THE USER HAS PROVIDED SCALING.
  220 CALL SCOPY(NCOLS,RW,1,X(NCOLS+MCON+1),1)
  230 CONTINUE
      DO 240 J = NCOLS + 1,NCOLS + MCON
          X(NCOLS+MCON+J) = ONE
  240 CONTINUE
C
C     WEIGHT THE LEAST SQUARES EQUATIONS.
      WT = SRELPR
      IF (ANORM.GT.ZERO) WT = WT/ANORM
      IF (CNORM.GT.ZERO) WT = WT*CNORM
      DO 250 I = 1,MOUT
          CALL SSCAL(NCOLS,WT,W(I+MCON,1),MDW)
  250 CONTINUE
      CALL SSCAL(MOUT,WT,W(MCON+1,MCON+NCOLS+1),1)
      LRW = 1
      LIW = 1
C
C     SET THE NEW TRIANGULARIZATION FACTOR.
      X(2* (NCOLS+MCON)+1) = ZERO
C
C     SET THE WEIGHT TO USE IN COMPONENTS .GT. MCON,
C     WHEN MAKING LINEAR INDEPENDENCE TEST.
      X(2* (NCOLS+MCON)+2) = ONE/WT
      M=MOUT+MCON
      CALL SBOLS(W,MDW,M,NCOLS+MCON,BL,BU,IND,IOPT(LBOU),X,
     *           RNORM,MODE,RW(LRW),IW(LIW))
      RNORM = RNORM/WT
C     END PROCEDURE
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  260 IF(MODE.GE.0)MODE = -NERR
      IGO = 0
      RETURN
C     END PROGRAM
      END
