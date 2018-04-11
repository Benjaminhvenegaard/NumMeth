      SUBROUTINE BVSUP (Y, NROWY, NCOMP, XPTS, NXPTS, A, NROWA, ALPHA,
     +   NIC, B, NROWB, BETA, NFC, IGOFX, RE, AE, IFLAG, WORK, NDW,
     +   IWORK, NDIW, NEQIVP)
C **********************************************************************
C
C
      DIMENSION Y(NROWY,*),A(NROWA,*),ALPHA(*),B(NROWB,*),
     1          BETA(*),WORK(*),IWORK(*),XPTS(*)
      CHARACTER*8 XERN1, XERN2, XERN3, XERN4
C
C **********************************************************************
C     THE COMMON BLOCK BELOW IS USED TO COMMUNICATE WITH SUBROUTINE
C     BVDER.  THE USER SHOULD NOT ALTER OR USE THIS COMMON BLOCK IN THE
C     CALLING PROGRAM.
C
      COMMON /ML8SZ/ C,XSAV,IGOFXD,INHOMO,IVP,NCOMPD,NFCD
C
C **********************************************************************
C     THESE COMMON BLOCKS AID IN REDUCING THE NUMBER OF SUBROUTINE
C     ARGUMENTS PREVALENT IN THIS MODULAR STRUCTURE
C
      COMMON /ML18JR/ AED,RED,TOL,NXPTSD,NICD,NOPG,MXNON,NDISK,NTAPE,
     1                NEQ,INDPVT,INTEG,NPS,NTP,NEQIVD,NUMORT,NFCC,
     2                ICOCO
      COMMON /ML17BW/ KKKZPW,NEEDW,NEEDIW,K1,K2,K3,K4,K5,K6,K7,K8,K9,
     1                K10,K11,L1,L2,KKKINT,LLLINT
C
C **********************************************************************
C     THIS COMMON BLOCK IS USED IN SUBROUTINES BVSUP,BVPOR,RKFAB,
C     REORT, AND STWAY. IT CONTAINS INFORMATION NECESSARY
C     FOR THE ORTHONORMALIZATION TESTING PROCEDURE AND A BACKUP
C     RESTARTING CAPABILITY.
C
      COMMON /ML15TO/ PX,PWCND,TND,X,XBEG,XEND,XOT,XOP,INFO(15),ISTKOP,
     1                KNSWOT,KOP,LOTJP,MNSWOT,NSWOT
C
C **********************************************************************
C     THIS COMMON BLOCK CONTAINS THE MACHINE DEPENDENT PARAMETERS
C     USED BY THE CODE
C
      COMMON /ML5MCO/ URO,SRU,EPS,SQOVFL,TWOU,FOURU,LPAR
C
C **********************************************************************
C     SET UP MACHINE DEPENDENT CONSTANTS.
C
C***FIRST EXECUTABLE STATEMENT  BVSUP
      CALL MACON
C
C **********************************************************************
C     TEST FOR INVALID INPUT
C
      IF (NROWY .LT. NCOMP)  GO TO 20
      IF (NCOMP .NE. NIC+NFC)  GO TO 20
      IF (NXPTS .LT. 2)  GO TO 20
      IF (NIC .LE. 0)  GO TO 20
      IF (NROWA .LT. NIC)  GO TO 20
      IF (NFC .LE. 0)  GO TO 20
      IF (NROWB .LT. NFC)  GO TO 20
      IF (IGOFX .LT. 0  .OR.  IGOFX .GT. 1) GO TO 20
      IF (RE .LT. 0.0)  GO TO 20
      IF (AE .LT. 0.0)  GO TO 20
      IF (RE .EQ. 0.0  .AND.  AE .EQ. 0.0)  GO TO 20
      IS = 1
      IF (XPTS(NXPTS) .LT. XPTS(1))  IS = 2
      NXPTSM = NXPTS - 1
      DO 13 K = 1,NXPTSM
      IF (IS .EQ. 2) GO TO 12
      IF (XPTS(K+1) .LE. XPTS(K))  GO TO 20
      GO TO 13
   12 IF (XPTS(K) .LE. XPTS(K+1))  GO TO 20
   13 CONTINUE
      GO TO 30
   20 IFLAG = -2
      RETURN
   30 CONTINUE
C
C **********************************************************************
C     CHECK FOR DISK STORAGE
C
      KPTS = NXPTS
      NDISK = 0
      IF (IWORK(12) .EQ. 0)  GO TO 35
      NTAPE = IWORK(12)
      KPTS = 1
      NDISK = 1
   35 CONTINUE
C
C **********************************************************************
C     SET INTEG PARAMETER ACCORDING TO CHOICE OF INTEGRATOR.
C
      INTEG = 1
      IF (IWORK(9) .EQ. 2)  INTEG = 2
C
C **********************************************************************
C     COMPUTE INHOMO
C
      IF (IGOFX .EQ. 1)  GO TO 43
      DO 40 J = 1,NIC
      IF (ALPHA(J) .NE. 0.0)  GO TO 43
   40 CONTINUE
      DO 41 J = 1,NFC
      IF (BETA(J) .NE. 0.0)  GO TO 42
   41 CONTINUE
      INHOMO = 3
      GO TO 45
   42 INHOMO = 2
      GO TO 45
   43 INHOMO = 1
   45 CONTINUE
C
C **********************************************************************
C     TO TAKE ADVANTAGE OF THE SPECIAL STRUCTURE WHEN SOLVING A
C     COMPLEX VALUED PROBLEM,WE INTRODUCE NFCC=NFC WHILE CHANGING
C     THE INTERNAL VALUE OF NFC
C
      NFCC=NFC
      IF (IFLAG .EQ. 13) NFC=NFC/2
C
C **********************************************************************
C     DETERMINE NECESSARY STORAGE REQUIREMENTS
C
C FOR BASIC ARRAYS IN BVPOR
      KKKYHP = NCOMP*(NFC+1) + NEQIVP
      KKKU   = NCOMP*NFC*KPTS
      KKKV   = NCOMP*KPTS
      KKKCOE = NFCC
      KKKS   = NFC+1
      KKKSTO = NCOMP*(NFC+1) + NEQIVP + 1
      KKKG   = NCOMP
C
C FOR ORTHONORMALIZATION RELATED MATTERS
      NTP = (NFCC*(NFCC+1))/2
      KKKZPW = 1 + NTP + NFCC
      LLLIP  = NFCC
C
C FOR ADDITIONAL REQUIRED WORK SPACE
C   (LSSUDS)
      KKKSUD = 4*NIC + (NROWA+1)*NCOMP
      LLLSUD = NIC
C   (SVECS)
      KKKSVC = 1 + 4*NFCC + 2*NFCC**2
      LLLSVC = 2*NFCC
C
      NDEQ=NCOMP*NFC+NEQIVP
      IF (INHOMO .EQ. 1) NDEQ=NDEQ+NCOMP
      GO TO (51,52),INTEG
C   (DERKF)
   51 KKKINT = 33 + 7*NDEQ
      LLLINT = 34
      GO TO 55
C   (DEABM)
   52 KKKINT = 130 + 21*NDEQ
      LLLINT = 51
C
C   (COEF)
   55 KKKCOF = 5*NFCC + NFCC**2
      LLLCOF = 3 + NFCC
C
      KKKWS  = MAX(KKKSUD,KKKSVC,KKKINT,KKKCOF)
      LLLIWS = MAX(LLLSUD,LLLSVC,LLLINT,LLLCOF)
C
      NEEDW  = KKKYHP + KKKU + KKKV + KKKCOE + KKKS + KKKSTO + KKKG +
     1         KKKZPW + KKKWS
      NEEDIW = 17 + LLLIP + LLLIWS
C **********************************************************************
C     COMPUTE THE NUMBER OF POSSIBLE ORTHONORMALIZATIONS WITH THE
C     ALLOTTED STORAGE
C
      IWORK(3) = NEEDW
      IWORK(4) = KKKZPW
      IWORK(5) = NEEDIW
      IWORK(6) = LLLIP
      NRTEMP = NDW - NEEDW
      NITEMP = NDIW - NEEDIW
      IF (NRTEMP .LT. 0)  GO TO 70
      IF (NITEMP .GE. 0)  GO TO 75
C
   70 IFLAG = -1
      IF (NDISK .NE. 1) THEN
         WRITE (XERN1, '(I8)') NEEDW
         WRITE (XERN2, '(I8)') KKKZPW
         WRITE (XERN3, '(I8)') NEEDIW
         WRITE (XERN4, '(I8)') LLLIP
         CALL XERMSG ('SLATEC', 'BVSUP',
     *      'REQUIRED STORAGE FOR WORK ARRAY IS '  // XERN1 // ' + ' //
     *      XERN2 // '*(EXPECTED NUMBER OF ORTHONORMALIZATIONS) $$'  //
     *      'REQUIRED STORAGE FOR IWORK ARRAY IS ' // XERN3 // ' + ' //
     *      XERN4 // '*(EXPECTED NUMBER OF ORTHONORMALIZATIONS)', 1, 0)
      ELSE
         WRITE (XERN1, '(I8)') NEEDW
         WRITE (XERN2, '(I8)') NEEDIW
         CALL XERMSG ('SLATEC', 'BVSUP',
     *      'REQUIRED STORAGE FOR WORK ARRAY IS '  // XERN1 //
     *      ' + NUMBER OF ORTHONOMALIZATIONS. $$'  //
     *      'REQUIRED STORAGE FOR IWORK ARRAY IS ' // XERN2, 1, 0)
      ENDIF
      RETURN
C
   75 IF (NDISK .EQ. 0)  GO TO 77
      NON = 0
      MXNON = NRTEMP
      GO TO 78
C
   77 MXNONR = NRTEMP / KKKZPW
      MXNONI = NITEMP / LLLIP
      MXNON = MIN(MXNONR,MXNONI)
      NON = MXNON
C
   78 IWORK(2) = MXNON
C
C **********************************************************************
C     CHECK FOR PRE-ASSIGNED ORTHONORMALIZATION POINTS
C
      NOPG = 0
      IF (IWORK(11) .NE. 1)  GO TO 85
      IF (MXNON .LT. IWORK(1))  GO TO 70
      NOPG = 1
      MXNON = IWORK(1)
      WORK(MXNON+1) = 2. * XPTS(NXPTS)  -  XPTS(1)
   85 CONTINUE
C
C **********************************************************************
C     ALLOCATE STORAGE FROM WORK AND IWORK ARRAYS
C
C  (Z)
      K1 = 1 + (MXNON+1)
C  (P)
      K2 = K1 + NTP*(NON+1)
C  (W)
      K3 = K2 + NFCC*(NON+1)
C  (YHP)
      K4 = K3 + KKKYHP
C  (U)
      K5 = K4 + KKKU
C  (V)
      K6 = K5 + KKKV
C  (COEF)
      K7 = K6 + KKKCOE
C  (S)
      K8 = K7 + KKKS
C  (STOWA)
      K9 = K8 + KKKSTO
C  (G)
      K10 = K9 + KKKG
      K11 = K10 + KKKWS
C            REQUIRED ADDITIONAL REAL WORK SPACE STARTS AT WORK(K10)
C            AND EXTENDS TO WORK(K11-1)
C
C     FIRST 17 LOCATIONS OF IWORK ARE USED FOR OPTIONAL
C     INPUT AND OUTPUT ITEMS
C  (IP)
      L1 = 18 + NFCC*(NON+1)
      L2 = L1 + LLLIWS
C            REQUIRED INTEGER WORK SPACE STARTS AT IWORK(L1)
C            AND EXTENDS TO IWORK(L2-1)
C
C **********************************************************************
C     SET INDICATOR FOR NORMALIZATION OF PARTICULAR SOLUTION
C
      NPS = 0
      IF (IWORK(10) .EQ. 1)  NPS = 1
C
C **********************************************************************
C     SET PIVOTING PARAMETER
C
      INDPVT=0
      IF (IWORK(15) .EQ. 1) INDPVT=1
C
C **********************************************************************
C     SET OTHER COMMON BLOCK PARAMETERS
C
      NFCD = NFC
      NCOMPD = NCOMP
      IGOFXD = IGOFX
      NXPTSD = NXPTS
      NICD = NIC
      RED = RE
      AED = AE
      NEQIVD = NEQIVP
      MNSWOT = 20
      IF (IWORK(13) .EQ. -1) MNSWOT=MAX(1,IWORK(14))
      XBEG=XPTS(1)
      XEND=XPTS(NXPTS)
      XSAV=XEND
      ICOCO=1
      IF (INHOMO .EQ. 3  .AND.  NOPG .EQ. 1) WORK(MXNON+1)=XEND
C
C **********************************************************************
C
      CALL EXBVP(Y,NROWY,XPTS,A,NROWA,ALPHA,B,NROWB,BETA,IFLAG,WORK,
     1           IWORK)
      NFC=NFCC
      IWORK(17)=IWORK(L1)
      RETURN
      END
