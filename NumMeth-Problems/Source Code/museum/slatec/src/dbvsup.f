      SUBROUTINE DBVSUP (Y, NROWY, NCOMP, XPTS, NXPTS, A, NROWA, ALPHA,
     +   NIC, B, NROWB, BETA, NFC, IGOFX, RE, AE, IFLAG, WORK, NDW,
     +   IWORK, NDIW, NEQIVP)
C **********************************************************************
C
      INTEGER ICOCO, IFLAG, IGOFX, IGOFXD, INDPVT, INFO, INHOMO, INTEG,
     1     IS, ISTKOP, IVP, IWORK(*), J, K, K1, K10, K11, K2,
     2     K3, K4, K5, K6, K7, K8, K9, KKKCOE, KKKCOF, KKKG, KKKINT,
     3     KKKS, KKKSTO, KKKSUD, KKKSVC, KKKU, KKKV, KKKWS, KKKYHP,
     4     KKKZPW, KNSWOT, KOP, KPTS, L1, L2, LLLCOF, LLLINT, LLLIP,
     5     LLLIWS, LLLSUD, LLLSVC, LOTJP, LPAR, MNSWOT,
     6     MXNON, MXNONI, MXNONR, NCOMP, NCOMPD, NDEQ, NDISK, NDIW,
     7     NDW, NEEDIW, NEEDW, NEQ, NEQIVD, NEQIVP, NFC, NFCC,
     8     NFCD, NIC, NICD, NITEMP, NON, NOPG, NPS, NROWA, NROWB,
     9     NROWY, NRTEMP, NSWOT, NTAPE, NTP, NUMORT, NXPTS, NXPTSD,
     1     NXPTSM
      DOUBLE PRECISION A(NROWA,*), AE, AED, ALPHA(*),
     1     B(NROWB,*), BETA(*), C, EPS, FOURU, PWCND, PX, RE,
     2     RED, SQOVFL, SRU, TND, TOL, TWOU, URO, WORK(NDW), X, XBEG,
     3     XEND, XOP, XOT, XPTS(*), XSAV, Y(NROWY,*)
      CHARACTER*8 XERN1, XERN2, XERN3, XERN4
C
C     ******************************************************************
C         THE COMMON BLOCK BELOW IS USED TO COMMUNICATE WITH SUBROUTINE
C         DBVDER.  THE USER SHOULD NOT ALTER OR USE THIS COMMON BLOCK IN
C         THE CALLING PROGRAM.
C
      COMMON /DML8SZ/ C,XSAV,IGOFXD,INHOMO,IVP,NCOMPD,NFCD
C
C     ******************************************************************
C         THESE COMMON BLOCKS AID IN REDUCING THE NUMBER OF SUBROUTINE
C         ARGUMENTS PREVALENT IN THIS MODULAR STRUCTURE
C
      COMMON /DML18J/ AED,RED,TOL,NXPTSD,NICD,NOPG,MXNON,NDISK,NTAPE,
     1                NEQ,INDPVT,INTEG,NPS,NTP,NEQIVD,NUMORT,NFCC,
     2                ICOCO
      COMMON /DML17B/ KKKZPW,NEEDW,NEEDIW,K1,K2,K3,K4,K5,K6,K7,K8,K9,
     1                K10,K11,L1,L2,KKKINT,LLLINT
C
C     ******************************************************************
C         THIS COMMON BLOCK IS USED IN SUBROUTINES DBVSUP,DBVPOR,DRKFAB,
C         DREORT, AND DSTWAY. IT CONTAINS INFORMATION NECESSARY
C         FOR THE ORTHONORMALIZATION TESTING PROCEDURE AND A BACKUP
C         RESTARTING CAPABILITY.
C
      COMMON /DML15T/ PX,PWCND,TND,X,XBEG,XEND,XOT,XOP,INFO(15),ISTKOP,
     1                KNSWOT,KOP,LOTJP,MNSWOT,NSWOT
C
C     ******************************************************************
C         THIS COMMON BLOCK CONTAINS THE MACHINE DEPENDENT PARAMETERS
C         USED BY THE CODE
C
      COMMON /DML5MC/ URO,SRU,EPS,SQOVFL,TWOU,FOURU,LPAR
C
C      *****************************************************************
C          SET UP MACHINE DEPENDENT CONSTANTS.
C
C***FIRST EXECUTABLE STATEMENT  DBVSUP
                        CALL DMACON
C
C                       ************************************************
C                           TEST FOR INVALID INPUT
C
                        IF (NROWY .LT. NCOMP) GO TO 80
                        IF (NCOMP .NE. NIC + NFC) GO TO 80
                        IF (NXPTS .LT. 2) GO TO 80
                        IF (NIC .LE. 0) GO TO 80
                        IF (NROWA .LT. NIC) GO TO 80
                        IF (NFC .LE. 0) GO TO 80
                        IF (NROWB .LT. NFC) GO TO 80
                        IF (IGOFX .LT. 0 .OR. IGOFX .GT. 1) GO TO 80
                        IF (RE .LT. 0.0D0) GO TO 80
                        IF (AE .LT. 0.0D0) GO TO 80
                        IF (RE .EQ. 0.0D0 .AND. AE .EQ. 0.0D0) GO TO 80
C                          BEGIN BLOCK PERMITTING ...EXITS TO 70
                              IS = 1
                              IF (XPTS(NXPTS) .LT. XPTS(1)) IS = 2
                              NXPTSM = NXPTS - 1
                              DO 30 K = 1, NXPTSM
                                 IF (IS .EQ. 2) GO TO 10
C                          .........EXIT
                                    IF (XPTS(K+1) .LE. XPTS(K)) GO TO 70
                                 GO TO 20
   10                            CONTINUE
C                          .........EXIT
                                    IF (XPTS(K) .LE. XPTS(K+1)) GO TO 70
   20                            CONTINUE
   30                         CONTINUE
C
C                             ******************************************
C                                 CHECK FOR DISK STORAGE
C
                              KPTS = NXPTS
                              NDISK = 0
                              IF (IWORK(12) .EQ. 0) GO TO 40
                                 NTAPE = IWORK(12)
                                 KPTS = 1
                                 NDISK = 1
   40                         CONTINUE
C
C                             ******************************************
C                                 SET INTEG PARAMETER ACCORDING TO
C                                 CHOICE OF INTEGRATOR.
C
                              INTEG = 1
                              IF (IWORK(9) .EQ. 2) INTEG = 2
C
C                             ******************************************
C                                 COMPUTE INHOMO
C
C                 ............EXIT
                              IF (IGOFX .EQ. 1) GO TO 100
                              DO 50 J = 1, NIC
C                 ...............EXIT
                                 IF (ALPHA(J) .NE. 0.0D0) GO TO 100
   50                         CONTINUE
                              DO 60 J = 1, NFC
C                    ............EXIT
                                 IF (BETA(J) .NE. 0.0D0) GO TO 90
   60                         CONTINUE
                              INHOMO = 3
C              ...............EXIT
                              GO TO 110
   70                      CONTINUE
   80                   CONTINUE
                        IFLAG = -2
C     ..................EXIT
                        GO TO 220
   90                CONTINUE
                     INHOMO = 2
C              ......EXIT
                     GO TO 110
  100             CONTINUE
                  INHOMO = 1
  110          CONTINUE
C
C              *********************************************************
C                  TO TAKE ADVANTAGE OF THE SPECIAL STRUCTURE WHEN
C                  SOLVING A COMPLEX*16 VALUED PROBLEM,WE INTRODUCE
C                  NFCC=NFC WHILE CHANGING THE INTERNAL VALUE OF NFC
C
               NFCC = NFC
               IF (IFLAG .EQ. 13) NFC = NFC/2
C
C              *********************************************************
C                  DETERMINE NECESSARY STORAGE REQUIREMENTS
C
C              FOR BASIC ARRAYS IN DBVPOR
               KKKYHP = NCOMP*(NFC + 1) + NEQIVP
               KKKU = NCOMP*NFC*KPTS
               KKKV = NCOMP*KPTS
               KKKCOE = NFCC
               KKKS = NFC + 1
               KKKSTO = NCOMP*(NFC + 1) + NEQIVP + 1
               KKKG = NCOMP
C
C              FOR ORTHONORMALIZATION RELATED MATTERS
               NTP = (NFCC*(NFCC + 1))/2
               KKKZPW = 1 + NTP + NFCC
               LLLIP = NFCC
C
C              FOR ADDITIONAL REQUIRED WORK SPACE
C                (DLSSUD)
               KKKSUD = 4*NIC + (NROWA + 1)*NCOMP
               LLLSUD = NIC
C              (DVECS)
               KKKSVC = 1 + 4*NFCC + 2*NFCC**2
               LLLSVC = 2*NFCC
C
               NDEQ = NCOMP*NFC + NEQIVP
               IF (INHOMO .EQ. 1) NDEQ = NDEQ + NCOMP
               GO TO (120,130), INTEG
C              (DDERKF)
  120          CONTINUE
                  KKKINT = 33 + 7*NDEQ
                  LLLINT = 34
               GO TO 140
C              (DDEABM)
  130          CONTINUE
                  KKKINT = 130 + 21*NDEQ
                  LLLINT = 51
  140          CONTINUE
C
C              (COEF)
               KKKCOF = 5*NFCC + NFCC**2
               LLLCOF = 3 + NFCC
C
               KKKWS = MAX(KKKSUD,KKKSVC,KKKINT,KKKCOF)
               LLLIWS = MAX(LLLSUD,LLLSVC,LLLINT,LLLCOF)
C
               NEEDW = KKKYHP + KKKU + KKKV + KKKCOE + KKKS + KKKSTO
     1                 + KKKG + KKKZPW + KKKWS
               NEEDIW = 17 + LLLIP + LLLIWS
C              *********************************************************
C                  COMPUTE THE NUMBER OF POSSIBLE ORTHONORMALIZATIONS
C                  WITH THE ALLOTTED STORAGE
C
               IWORK(3) = NEEDW
               IWORK(4) = KKKZPW
               IWORK(5) = NEEDIW
               IWORK(6) = LLLIP
               NRTEMP = NDW - NEEDW
               NITEMP = NDIW - NEEDIW
C           ...EXIT
               IF (NRTEMP .LT. 0) GO TO 180
C           ...EXIT
               IF (NITEMP .LT. 0) GO TO 180
C
               IF (NDISK .EQ. 0) GO TO 150
                  NON = 0
                  MXNON = NRTEMP
               GO TO 160
  150          CONTINUE
C
                  MXNONR = NRTEMP/KKKZPW
                  MXNONI = NITEMP/LLLIP
                  MXNON = MIN(MXNONR,MXNONI)
                  NON = MXNON
  160          CONTINUE
C
               IWORK(2) = MXNON
C
C              *********************************************************
C                  CHECK FOR PRE-ASSIGNED ORTHONORMALIZATION POINTS
C
               NOPG = 0
C        ......EXIT
               IF (IWORK(11) .NE. 1) GO TO 210
               IF (MXNON .LT. IWORK(1)) GO TO 170
                  NOPG = 1
                  MXNON = IWORK(1)
                  WORK(MXNON+1) = 2.0D0*XPTS(NXPTS) - XPTS(1)
C        .........EXIT
                  GO TO 210
  170          CONTINUE
  180       CONTINUE
C
            IFLAG = -1
      IF (NDISK .NE. 1) THEN
         WRITE (XERN1, '(I8)') NEEDW
         WRITE (XERN2, '(I8)') KKKZPW
         WRITE (XERN3, '(I8)') NEEDIW
         WRITE (XERN4, '(I8)') LLLIP
         CALL XERMSG ('SLATEC', 'DBVSUP',
     *      'REQUIRED STORAGE FOR WORK ARRAY IS '  // XERN1 // ' + ' //
     *      XERN2 // '*(EXPECTED NUMBER OF ORTHONORMALIZATIONS) $$'  //
     *      'REQUIRED STORAGE FOR IWORK ARRAY IS ' // XERN3 // ' + ' //
     *      XERN4 // '*(EXPECTED NUMBER OF ORTHONORMALIZATIONS)', 1, 0)
      ELSE
         WRITE (XERN1, '(I8)') NEEDW
         WRITE (XERN2, '(I8)') NEEDIW
         CALL XERMSG ('SLATEC', 'DBVSUP',
     *      'REQUIRED STORAGE FOR WORK ARRAY IS '  // XERN1 //
     *      ' + NUMBER OF ORTHONOMALIZATIONS. $$'  //
     *      'REQUIRED STORAGE FOR IWORK ARRAY IS ' // XERN2, 1, 0)
      ENDIF
      RETURN
C
C        ***************************************************************
C            ALLOCATE STORAGE FROM WORK AND IWORK ARRAYS
C
C         (Z)
  210    K1 = 1 + (MXNON + 1)
C        (P)
         K2 = K1 + NTP*(NON + 1)
C        (W)
         K3 = K2 + NFCC*(NON + 1)
C        (YHP)
         K4 = K3 + KKKYHP
C        (U)
         K5 = K4 + KKKU
C        (V)
         K6 = K5 + KKKV
C        (COEF)
         K7 = K6 + KKKCOE
C        (S)
         K8 = K7 + KKKS
C        (STOWA)
         K9 = K8 + KKKSTO
C        (G)
         K10 = K9 + KKKG
         K11 = K10 + KKKWS
C                  REQUIRED ADDITIONAL DOUBLE PRECISION WORK SPACE
C                  STARTS AT WORK(K10) AND EXTENDS TO WORK(K11-1)
C
C           FIRST 17 LOCATIONS OF IWORK ARE USED FOR OPTIONAL
C           INPUT AND OUTPUT ITEMS
C        (IP)
         L1 = 18 + NFCC*(NON + 1)
         L2 = L1 + LLLIWS
C                   REQUIRED INTEGER WORK SPACE STARTS AT IWORK(L1)
C                   AND EXTENDS TO IWORK(L2-1)
C
C        ***************************************************************
C            SET INDICATOR FOR NORMALIZATION OF PARTICULAR SOLUTION
C
         NPS = 0
         IF (IWORK(10) .EQ. 1) NPS = 1
C
C        ***************************************************************
C            SET PIVOTING PARAMETER
C
         INDPVT = 0
         IF (IWORK(15) .EQ. 1) INDPVT = 1
C
C        ***************************************************************
C            SET OTHER COMMON BLOCK PARAMETERS
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
         IF (IWORK(13) .EQ. -1) MNSWOT = MAX(1,IWORK(14))
         XBEG = XPTS(1)
         XEND = XPTS(NXPTS)
         XSAV = XEND
         ICOCO = 1
         IF (INHOMO .EQ. 3 .AND. NOPG .EQ. 1) WORK(MXNON+1) = XEND
C
C        ***************************************************************
C
         CALL DEXBVP(Y,NROWY,XPTS,A,NROWA,ALPHA,B,NROWB,BETA,IFLAG,WORK,
     1               IWORK)
         NFC = NFCC
         IWORK(17) = IWORK(L1)
  220 CONTINUE
      RETURN
      END
