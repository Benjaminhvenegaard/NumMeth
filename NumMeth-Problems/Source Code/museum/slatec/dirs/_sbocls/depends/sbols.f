      SUBROUTINE SBOLS (W, MDW, MROWS, NCOLS, BL, BU, IND, IOPT, X,
     +   RNORM, MODE, RW, IW)
C
C     SOLVE LINEAR LEAST SQUARES SYSTEM WITH BOUNDS ON
C     SELECTED VARIABLES.
C     REVISED 850329-1400
C     REVISED YYMMDD-HHMM
C     TO CHANGE THIS SUBPROGRAM FROM SINGLE TO DOUBLE PRECISION BEGIN
C     EDITING AT THE CARD 'C++'.
C     CHANGE THIS SUBPROGRAM NAME TO DBOLS AND THE STRINGS
C     /SCOPY/ TO /DCOPY/, /SBOL/ TO /DBOL/,
C     /SNRM2/ TO /DNRM2/, /ISAMAX/ TO /IDAMAX/,
C     /SROTG/ TO /DROTG/, /SROT/ TO /DROT/, /E0/ TO /D0/,
C     /REAL            / TO /DOUBLE PRECISION/.
C ++
      REAL             W(MDW,*),BL(*),BU(*),X(*),RW(*)
      REAL             SC, SS, ONE, SNRM2, RNORM, ZERO
C
C     THIS VARIABLE SHOULD REMAIN TYPE REAL.
      INTEGER IND(*),IOPT(*),IW(*)
      LOGICAL CHECKL
      CHARACTER*8 XERN1, XERN2
      CHARACTER*16 XERN3, XERN4
      SAVE IGO,LOCACC,LOPT,ISCALE
      DATA IGO/0/
C***FIRST EXECUTABLE STATEMENT  SBOLS
      NERR = 0
      MODE = 0
      IF (IGO.EQ.0) THEN
C     DO(CHECK VALIDITY OF INPUT DATA)
C     PROCEDURE(CHECK VALIDITY OF INPUT DATA)
C
C     SEE THAT MDW IS .GT.0. GROSS CHECK ONLY.
          IF (MDW.LE.0) THEN
              WRITE (XERN1, '(I8)') MDW
              CALL XERMSG ('SLATEC', 'SBOLS', 'MDW = ' // XERN1 //
     *           ' MUST BE POSITIVE.', 2, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          ENDIF
C
C     SEE THAT NUMBER OF UNKNOWNS IS POSITIVE.
          IF (NCOLS.LE.0) THEN
              WRITE (XERN1, '(I8)') NCOLS
              CALL XERMSG ('SLATEC', 'SBOLS', 'NCOLS = ' // XERN1 //
     *           ' THE NO. OF VARIABLES MUST BE POSITIVE.', 3, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          ENDIF
C
C     SEE THAT CONSTRAINT INDICATORS ARE ALL WELL-DEFINED.
          DO 10 J = 1,NCOLS
              IF (IND(J).LT.1 .OR. IND(J).GT.4) THEN
                  WRITE (XERN1, '(I8)') J
                  WRITE (XERN2, '(I8)') IND(J)
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'IND(' // XERN1 // ') = ' // XERN2 //
     *               ' MUST BE 1-4.', 4, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
   10     CONTINUE
C
C     SEE THAT BOUNDS ARE CONSISTENT.
          DO 20 J = 1,NCOLS
              IF (IND(J).EQ.3) THEN
                  IF (BL(J).GT.BU(J)) THEN
                      WRITE (XERN1, '(I8)') J
                      WRITE (XERN3, '(1PE15.6)') BL(J)
                      WRITE (XERN4, '(1PE15.6)') BU(J)
                      CALL XERMSG ('SLATEC', 'SBOLS',
     *                   'BOUND BL(' // XERN1 // ') = ' // XERN3 //
     *                   ' IS .GT. BU(' // XERN1 // ') = ' // XERN4,
     *                   5, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  ENDIF
              ENDIF
   20     CONTINUE
C     END PROCEDURE
C     DO(PROCESS OPTION ARRAY)
C     PROCEDURE(PROCESS OPTION ARRAY)
          ZERO = 0.E0
          ONE = 1.E0
          CHECKL = .FALSE.
          LENX = NCOLS
          ISCALE = 1
          IGO = 2
          LOPT = 0
          LP = 0
          LDS = 0
   30     CONTINUE
          LP = LP + LDS
          IP = IOPT(LP+1)
          JP = ABS(IP)
C
C     TEST FOR NO MORE OPTIONS.
          IF (IP.EQ.99) THEN
              IF (LOPT.EQ.0) LOPT = LP + 1
              GO TO 50
          ELSE IF (JP.EQ.99) THEN
              LDS = 1
              GO TO 30
          ELSE IF (JP.EQ.1) THEN
              IF (IP.GT.0) THEN
C
C     SET UP DIRECTION FLAG, ROW STACKING POINTER
C     LOCATION, AND LOCATION FOR NUMBER OF NEW ROWS.
                  LOCACC = LP + 2
C
C                  IOPT(LOCACC-1)=OPTION NUMBER FOR SEQ. ACCUMULATION.
C     CONTENTS..   IOPT(LOCACC  )=USER DIRECTION FLAG, 1 OR 2.
C                  IOPT(LOCACC+1)=ROW STACKING POINTER.
C                  IOPT(LOCACC+2)=NUMBER OF NEW ROWS TO PROCESS.
C     USER ACTION WITH THIS OPTION..
C      (SET UP OPTION DATA FOR SEQ. ACCUMULATION IN IOPT(*).
C      MUST ALSO START PROCESS WITH IOPT(LOCACC)=1.)
C      (MOVE BLOCK OF EQUATIONS INTO W(*,*)  STARTING AT FIRST
C       ROW OF W(*,*).  SET IOPT(LOCACC+2)=NO. OF ROWS IN BLOCK.)
C              LOOP
C              CALL SBOLS()
C
C                  IF(IOPT(LOCACC) .EQ. 1) THEN
C                      STACK EQUAS., STARTING AT ROW IOPT(LOCACC+1),
C                       INTO W(*,*).
C                       SET IOPT(LOCACC+2)=NO. OF EQUAS.
C                      IF LAST BLOCK OF EQUAS., SET IOPT(LOCACC)=2.
C                  ELSE IF IOPT(LOCACC) .EQ. 2) THEN
C                      (PROCESS IS OVER. EXIT LOOP.)
C                  ELSE
C                      (ERROR CONDITION. SHOULD NOT HAPPEN.)
C                  END IF
C              END LOOP
C              SET IOPT(LOCACC-1)=-OPTION NUMBER FOR SEQ. ACCUMULATION.
C              CALL SBOLS( )
                  IOPT(LOCACC+1) = 1
                  IGO = 1
              ENDIF
              LDS = 4
              GO TO 30
          ELSE IF (JP.EQ.2) THEN
              IF (IP.GT.0) THEN
C
C     GET ACTUAL LENGTHS OF ARRAYS FOR CHECKING AGAINST NEEDS.
                  LOCDIM = LP + 2
C
C     LMDW.GE.MROWS
C     LNDW.GE.NCOLS+1
C     LLB .GE.NCOLS
C     LLX .GE.NCOLS+EXTRA REQD. IN OPTIONS.
C     LLRW.GE.5*NCOLS
C     LLIW.GE.2*NCOLS
C     LIOP.GE. AMOUNT REQD. FOR IOPTION ARRAY.
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
                      CALL XERMSG ('SLATEC', 'SBOLS', 'ISCALE OPTION = '
     *                   // XERN1 // ' MUST BE 1-3', 7, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
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
                      CALL XERMSG ('SLATEC', 'SBOLS',
     *                   'OFFSET PAST X(NCOLS) (' // XERN1 //
     *           ') FOR USER-PROVIDED COLUMN SCALING MUST BE POSITIVE.',
     *                   8, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                      GO TO 190
                  ENDIF
                  CALL SCOPY(NCOLS,X(NCOLS+IOPT(LP+2)),1,RW,1)
                  LENX = LENX + NCOLS
                  DO 40 J = 1,NCOLS
                      IF (RW(J).LE.ZERO) THEN
                          WRITE (XERN1, '(I8)') J
                          WRITE (XERN3, '(1PE15.6)') RW(J)
                          CALL XERMSG ('SLATEC', 'SBOLS',
     *                       'EACH PROVIDED COLUMN SCALE FACTOR ' //
     *                       'MUST BE POSITIVE.$$COMPONENT ' // XERN1 //
     *                       ' NOW = ' // XERN3, 9, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                          GO TO 190
                      ENDIF
   40             CONTINUE
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     IN THIS OPTION AN OPTION ARRAY IS PROVIDED TO SBOLSM().
          ELSE IF (JP.EQ.5) THEN
              IF (IP.GT.0) THEN
                  LOPT = IOPT(LP+2)
              ENDIF
              LDS = 2
C     CYCLE FOREVER
              GO TO 30
C
C     THIS OPTION USES THE NEXT LOC OF IOPT(*) AS AN
C     INCREMENT TO SKIP.
          ELSE IF (JP.EQ.6) THEN
              IF (IP.GT.0) THEN
                  LP = IOPT(LP+2) - 1
                  LDS = 0
              ELSE
                  LDS = 2
              ENDIF
C     CYCLE FOREVER
              GO TO 30
C
C     NO VALID OPTION NUMBER WAS NOTED. THIS IS AN ERROR CONDITION.
          ELSE
              WRITE (XERN1, '(I8)') JP
              CALL XERMSG ('SLATEC', 'SBOLS', 'THE OPTION NUMBER = ' //
     *           XERN1 // ' IS NOT DEFINED.', 6, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
              GO TO 190
          ENDIF
   50     CONTINUE
C     END PROCEDURE
          IF (CHECKL) THEN
C     DO(CHECK LENGTHS OF ARRAYS)
C     PROCEDURE(CHECK LENGTHS OF ARRAYS)
C
C     THIS FEATURE ALLOWS THE USER TO MAKE SURE THAT THE
C     ARRAYS ARE LONG ENOUGH FOR THE INTENDED PROBLEM SIZE AND USE.
              IF (LMDW.LT.MROWS) THEN
                  WRITE (XERN1, '(I8)') LMDW
                  WRITE (XERN2, '(I8)') MROWS
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'THE ROW DIMENSION OF W(,) = ' // XERN1 //
     *               ' MUST BE .GE. THE NUMBER OF ROWS = ' // XERN2,
     *               11, 1)
                  GO TO 190
              ENDIF
              IF (LNDW.LT.NCOLS+1) THEN
                  WRITE (XERN1, '(I8)') LNDW
                  WRITE (XERN2, '(I8)') NCOLS+1
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'THE COLUMN DIMENSION OF W(,) = ' // XERN1 //
     *               ' MUST BE .GE. NCOLS+1 = ' // XERN2, 12, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
              IF (LLB.LT.NCOLS) THEN
                  WRITE (XERN1, '(I8)') LLB
                  WRITE (XERN2, '(I8)') NCOLS
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *           'THE DIMENSIONS OF THE ARRAYS BL(), BU(), AND IND() = '
     *               // XERN1 // ' MUST BE .GE. NCOLS = ' // XERN2,
     *               13, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
              IF (LLX.LT.LENX) THEN
                  WRITE (XERN1, '(I8)') LLX
                  WRITE (XERN2, '(I8)') LENX
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *              'THE DIMENSION OF X() = ' // XERN1 //
     *              ' MUST BE .GE. THE REQUIRED LENGTH = ' // XERN2,
     *              14, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
              IF (LLRW.LT.5*NCOLS) THEN
                  WRITE (XERN1, '(I8)') LLRW
                  WRITE (XERN2, '(I8)') 5*NCOLS
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'THE DIMENSION OF RW() = ' // XERN1 //
     *               ' MUST BE .GE. 5*NCOLS = ' // XERN2, 15, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
              IF (LLIW.LT.2*NCOLS) THEN
                  WRITE (XERN1, '(I8)') LLIW
                  WRITE (XERN2, '(I8)') 2*NCOLS
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'THE DIMENSION OF IW() = ' // XERN1 //
     *               ' MUST BE .GE. 2*NCOLS = ' // XERN2, 16, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
              IF (LIOPT.LT.LP+1) THEN
                  WRITE (XERN1, '(I8)') LIOPT
                  WRITE (XERN2, '(I8)') LP+1
                  CALL XERMSG ('SLATEC', 'SBOLS',
     *               'THE DIMENSION OF IOPT() = ' // XERN1 //
     *               ' MUST BE .GE. THE REQD. LEN = ' // XERN2, 17, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
                  GO TO 190
              ENDIF
C     END PROCEDURE
          ENDIF
      ENDIF
      GO TO (60,90),IGO
      GO TO 180
C
C     GO BACK TO THE USER FOR ACCUMULATION OF LEAST SQUARES
C     EQUATIONS AND DIRECTIONS TO QUIT PROCESSING.
C     CASE 1
   60 CONTINUE
C     DO(ACCUMULATE LEAST SQUARES EQUATIONS)
C     PROCEDURE(ACCUMULATE LEAST SQUARES EQUATIONS)
      MROWS = IOPT(LOCACC+1) - 1
      INROWS = IOPT(LOCACC+2)
      MNEW = MROWS + INROWS
      IF (MNEW.LT.0 .OR. MNEW.GT.MDW) THEN
          WRITE (XERN1, '(I8)') MNEW
          WRITE (XERN2, '(I8)') MDW
          CALL XERMSG ('SLATEC', 'SBOLS', 'NO. OF ROWS = ' // XERN1 //
     *       ' MUST BE .GE. 0 .AND. .LE. MDW = ' // XERN2, 10, 1)
C     DO(RETURN TO USER PROGRAM UNIT)
          GO TO 190
      ENDIF
      DO 80 J = 1,MIN(NCOLS+1,MNEW)
          DO 70 I = MNEW,MAX(MROWS,J) + 1,-1
              IBIG = ISAMAX(I-J,W(J,J),1) + J - 1
C
C     PIVOT FOR INCREASED STABILITY.
              CALL SROTG(W(IBIG,J),W(I,J),SC,SS)
              CALL SROT(NCOLS+1-J,W(IBIG,J+1),MDW,W(I,J+1),MDW,SC,SS)
              W(I,J) = ZERO
   70     CONTINUE
   80 CONTINUE
      MROWS = MIN(NCOLS+1,MNEW)
      IOPT(LOCACC+1) = MROWS + 1
      IGO = IOPT(LOCACC)
C     END PROCEDURE
      IF (IGO.EQ.2) THEN
          IGO = 0
      ENDIF
      GO TO 180
C     CASE 2
   90 CONTINUE
C     DO(INITIALIZE VARIABLES AND DATA VALUES)
C     PROCEDURE(INITIALIZE VARIABLES AND DATA VALUES)
      DO 150 J = 1,NCOLS
          GO TO (100,110,120,130),ISCALE
          GO TO 140
  100     CONTINUE
C     CASE 1
C
C     THIS IS THE NOMINAL SCALING. EACH NONZERO
C     COL. HAS MAX. NORM EQUAL TO ONE.
          IBIG = ISAMAX(MROWS,W(1,J),1)
          RW(J) = ABS(W(IBIG,J))
          IF (RW(J).EQ.ZERO) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          ENDIF
          GO TO 140
  110     CONTINUE
C     CASE 2
C
C     THIS CHOICE OF SCALING MAKES EACH NONZERO COLUMN
C     HAVE EUCLIDEAN LENGTH EQUAL TO ONE.
          RW(J) = SNRM2(MROWS,W(1,J),1)
          IF (RW(J).EQ.ZERO) THEN
              RW(J) = ONE
          ELSE
              RW(J) = ONE/RW(J)
          ENDIF
          GO TO 140
  120     CONTINUE
C     CASE 3
C
C     THIS CASE EFFECTIVELY SUPPRESSES SCALING BY SETTING
C     THE SCALING MATRIX TO THE IDENTITY MATRIX.
          RW(1) = ONE
          CALL SCOPY(NCOLS,RW,0,RW,1)
          GO TO 160
  130     CONTINUE
C     CASE 4
          GO TO 160
  140     CONTINUE
  150 CONTINUE
  160 CONTINUE
C     END PROCEDURE
C     DO(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C     PROCEDURE(SOLVE BOUNDED LEAST SQUARES PROBLEM)
C
C     INITIALIZE IBASIS(*), J=1,NCOLS, AND IBB(*), J=1,NCOLS,
C     TO =J,AND =1, FOR USE IN SBOLSM( ).
      DO 170 J = 1,NCOLS
          IW(J) = J
          IW(J+NCOLS) = 1
          RW(3*NCOLS+J) = BL(J)
          RW(4*NCOLS+J) = BU(J)
  170 CONTINUE
      CALL SBOLSM(W,MDW,MROWS,NCOLS,RW(3*NCOLS+1),RW(4*NCOLS+1),IND,
     .            IOPT(LOPT),X,RNORM,MODE,RW(NCOLS+1),RW(2*NCOLS+1),RW,
     .            IW,IW(NCOLS+1))
C     END PROCEDURE
      IGO = 0
  180 CONTINUE
      RETURN
C     PROCEDURE(RETURN TO USER PROGRAM UNIT)
  190 IF(MODE.GE.0)MODE = -NERR
      IGO = 0
      RETURN
C     END PROCEDURE
      END
