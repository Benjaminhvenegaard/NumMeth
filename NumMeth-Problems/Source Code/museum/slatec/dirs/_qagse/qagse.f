      SUBROUTINE QAGSE (F, A, B, EPSABS, EPSREL, LIMIT, RESULT, ABSERR,
     +   NEVAL, IER, ALIST, BLIST, RLIST, ELIST, IORD, LAST)
C
      REAL A,ABSEPS,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,
     1  A2,B,BLIST,B1,B2,CORREC,DEFABS,DEFAB1,DEFAB2,R1MACH,
     2  DRES,ELIST,EPMACH,EPSABS,EPSREL,ERLARG,ERLAST,ERRBND,
     3  ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,ERTEST,F,OFLOW,RESABS,
     4  RESEPS,RESULT,RES3LA,RLIST,RLIST2,SMALL,UFLOW
      INTEGER ID,IER,IERRO,IORD,IROFF1,IROFF2,IROFF3,JUPBND,K,KSGN,
     1  KTMIN,LAST,LIMIT,MAXERR,NEVAL,NRES,NRMAX,NUMRL2
      LOGICAL EXTRAP,NOEXT
C
      DIMENSION ALIST(*),BLIST(*),ELIST(*),IORD(*),
     1 RES3LA(3),RLIST(*),RLIST2(52)
C
      EXTERNAL F
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE QELG (RLIST2 SHOULD BE OF DIMENSION
C            (LIMEXP+2) AT LEAST).
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           RLIST2    - ARRAY OF DIMENSION AT LEAST LIMEXP+2
C                       CONTAINING THE PART OF THE EPSILON TABLE
C                       WHICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
C                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT INTERVAL
C           *****2    - VARIABLE FOR THE RIGHT INTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED
C                       UP TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
C                       IS ATTEMPTING TO PERFORM EXTRAPOLATION
C                       I.E. BEFORE SUBDIVIDING THE SMALLEST INTERVAL
C                       WE TRY TO DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE VALUE)
C
C            MACHINE DEPENDENT CONSTANTS
C            ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  QAGSE
      EPMACH = R1MACH(4)
C
C            TEST ON VALIDITY OF PARAMETERS
C            ------------------------------
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0E+00
      ABSERR = 0.0E+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0E+00
      ELIST(1) = 0.0E+00
      IF(EPSABS.LE.0.0E+00.AND.EPSREL.LT.MAX(0.5E+02*EPMACH,0.5E-14))
     1   IER = 6
      IF(IER.EQ.6) GO TO 999
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
      UFLOW = R1MACH(1)
      OFLOW = R1MACH(2)
      IERRO = 0
      CALL QK21(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
C
C           TEST ON ACCURACY.
C
      DRES = ABS(RESULT)
      ERRBND = MAX(EPSABS,EPSREL*DRES)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      IF(ABSERR.LE.1.0E+02*EPMACH*DEFABS.AND.ABSERR.GT.
     1  ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS).OR.
     1  ABSERR.EQ.0.0E+00) GO TO 140
C
C           INITIALIZATION
C           --------------
C
      RLIST2(1) = RESULT
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      ABSERR = OFLOW
      NRMAX = 1
      NRES = 0
      NUMRL2 = 2
      KTMIN = 0
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IROFF1 = 0
      IROFF2 = 0
      IROFF3 = 0
      KSGN = -1
      IF(DRES.GE.(0.1E+01-0.5E+02*EPMACH)*DEFABS) KSGN = 1
C
C           MAIN DO-LOOP
C           ------------
C
      DO 90 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH THE NRMAX-TH LARGEST
C           ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 0.5E+00*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        ERLAST = ERRMAX
        CALL QK21(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        CALL QK21(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
C           AND ERROR AND TEST FOR ACCURACY.
C
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2) GO TO 15
        IF(ABS(RLIST(MAXERR)-AREA12).GT.0.1E-04*ABS(AREA12)
     1  .OR.ERRO12.LT.0.99E+00*ERRMAX) GO TO 10
        IF(EXTRAP) IROFF2 = IROFF2+1
        IF(.NOT.EXTRAP) IROFF1 = IROFF1+1
   10   IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF3 = IROFF3+1
   15   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY
C           SET ERROR FLAG.
C
        IF(IROFF1+IROFF2.GE.10.OR.IROFF3.GE.20) IER = 2
        IF(IROFF2.GE.5) IERRO = 3
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
C           SUBINTERVALS EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT A POINT OF THE INTEGRATION RANGE.
C
        IF(MAX(ABS(A1),ABS(B2)).LE.(0.1E+01+0.1E+03*EPMACH)*
     1  (ABS(A2)+0.1E+04*UFLOW)) IER = 4
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
        IF(ERROR2.GT.ERROR1) GO TO 20
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 30
   20   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE QPSRT TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE
C           SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE
C           BISECTED NEXT).
C
   30   CALL QPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
C ***JUMP OUT OF DO-LOOP
        IF(ERRSUM.LE.ERRBND) GO TO 115
C ***JUMP OUT OF DO-LOOP
        IF(IER.NE.0) GO TO 100
        IF(LAST.EQ.2) GO TO 80
        IF(NOEXT) GO TO 90
        ERLARG = ERLARG-ERLAST
        IF(ABS(B1-A1).GT.SMALL) ERLARG = ERLARG+ERRO12
        IF(EXTRAP) GO TO 40
C
C           TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE
C           SMALLEST INTERVAL.
C
        IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
        EXTRAP = .TRUE.
        NRMAX = 2
   40   IF(IERRO.EQ.3.OR.ERLARG.LE.ERTEST) GO TO 60
C
C           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
C           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS
C           OVER THE LARGER INTERVALS (ERLARG) AND PERFORM
C           EXTRAPOLATION.
C
        ID = NRMAX
        JUPBND = LAST
        IF(LAST.GT.(2+LIMIT/2)) JUPBND = LIMIT+3-LAST
        DO 50 K = ID,JUPBND
          MAXERR = IORD(NRMAX)
          ERRMAX = ELIST(MAXERR)
C ***JUMP OUT OF DO-LOOP
          IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
          NRMAX = NRMAX+1
   50   CONTINUE
C
C           PERFORM EXTRAPOLATION.
C
   60   NUMRL2 = NUMRL2+1
        RLIST2(NUMRL2) = AREA
        CALL QELG(NUMRL2,RLIST2,RESEPS,ABSEPS,RES3LA,NRES)
        KTMIN = KTMIN+1
        IF(KTMIN.GT.5.AND.ABSERR.LT.0.1E-02*ERRSUM) IER = 5
        IF(ABSEPS.GE.ABSERR) GO TO 70
        KTMIN = 0
        ABSERR = ABSEPS
        RESULT = RESEPS
        CORREC = ERLARG
        ERTEST = MAX(EPSABS,EPSREL*ABS(RESEPS))
C ***JUMP OUT OF DO-LOOP
        IF(ABSERR.LE.ERTEST) GO TO 100
C
C           PREPARE BISECTION OF THE SMALLEST INTERVAL.
C
   70   IF(NUMRL2.EQ.1) NOEXT = .TRUE.
        IF(IER.EQ.5) GO TO 100
        MAXERR = IORD(1)
        ERRMAX = ELIST(MAXERR)
        NRMAX = 1
        EXTRAP = .FALSE.
        SMALL = SMALL*0.5E+00
        ERLARG = ERRSUM
        GO TO 90
   80   SMALL = ABS(B-A)*0.375E+00
        ERLARG = ERRSUM
        ERTEST = ERRBND
        RLIST2(2) = AREA
   90 CONTINUE
C
C           SET FINAL RESULT AND ERROR ESTIMATE.
C           ------------------------------------
C
  100 IF(ABSERR.EQ.OFLOW) GO TO 115
      IF(IER+IERRO.EQ.0) GO TO 110
      IF(IERRO.EQ.3) ABSERR = ABSERR+CORREC
      IF(IER.EQ.0) IER = 3
      IF(RESULT.NE.0.0E+00.AND.AREA.NE.0.0E+00) GO TO 105
      IF(ABSERR.GT.ERRSUM) GO TO 115
      IF(AREA.EQ.0.0E+00) GO TO 130
      GO TO 110
  105 IF(ABSERR/ABS(RESULT).GT.ERRSUM/ABS(AREA)) GO TO 115
C
C           TEST ON DIVERGENCE.
C
  110 IF(KSGN.EQ.(-1).AND.MAX(ABS(RESULT),ABS(AREA)).LE.
     1 DEFABS*0.1E-01) GO TO 130
      IF(0.1E-01.GT.(RESULT/AREA).OR.(RESULT/AREA).GT.0.1E+03
     1 .OR.ERRSUM.GT.ABS(AREA)) IER = 6
      GO TO 130
C
C           COMPUTE GLOBAL INTEGRAL SUM.
C
  115 RESULT = 0.0E+00
      DO 120 K = 1,LAST
         RESULT = RESULT+RLIST(K)
  120 CONTINUE
      ABSERR = ERRSUM
  130 IF(IER.GT.2) IER = IER-1
  140 NEVAL = 42*LAST-21
  999 RETURN
      END