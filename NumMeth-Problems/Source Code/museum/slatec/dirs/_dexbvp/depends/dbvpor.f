      SUBROUTINE DBVPOR (Y, NROWY, NCOMP, XPTS, NXPTS, A, NROWA, ALPHA,
     +   NIC, B, NROWB, BETA, NFC, IFLAG, Z, MXNON, P, NTP, IP, W, NIV,
     +   YHP, U, V, COEF, S, STOWA, G, WORK, IWORK, NFCC)
C
      DOUBLE PRECISION DDOT
      INTEGER I, I1, I2, IC, ICOCO, IFLAG, IGOFX, INDPVT, INFO, INHOMO,
     1     INTEG, IRA, ISFLG, ISTKOP, IVP, J,
     2     K, KNSWOT, KOD, KOP, KPTS, KWC, KWD, KWS, KWT, L, LOTJP, M,
     3     MNSWOT, MXNON, MXNOND, N, NCOMP, NCOMP2, NCOMPD, NDISK, NDW,
     4     NEQ, NEQIVP, NFC, NFCC, NFCCD, NFCD, NFCP1, NFCP2, NIC,
     5     NICD, NIV, NN, NON, NOPG, NPS, NROWA, NROWB, NROWY, NSWOT,
     6     NTAPE, NTP, NTPD, NUMORT, NXPTS, NXPTSD,
     7     IP(NFCC,*), IWORK(*)
      DOUBLE PRECISION A(NROWA,*), AE, ALPHA(*), B(NROWB,*),
     1     BETA(*), C, COEF(*), G(*), P(NTP,*), PWCND, PX,
     2     RE, S(*), STOWA(*), TND, TOL, U(NCOMP,NFC,*),
     3     V(NCOMP,*), W(NFCC,*), WORK(*), X, XBEG, XEND, XOP,
     4     XOT, XPTS(*), XSAV, Y(NROWY,*), YHP(NCOMP,*),
     5     Z(*)
C
C     ******************************************************************
C
      COMMON /DML8SZ/ C,XSAV,IGOFX,INHOMO,IVP,NCOMPD,NFCD
      COMMON /DML15T/ PX,PWCND,TND,X,XBEG,XEND,XOT,XOP,INFO(15),ISTKOP,
     1                KNSWOT,KOP,LOTJP,MNSWOT,NSWOT
      COMMON /DML18J/ AE,RE,TOL,NXPTSD,NICD,NOPG,MXNOND,NDISK,NTAPE,
     1                NEQ,INDPVT,INTEG,NPS,NTPD,NEQIVP,NUMORT,NFCCD,
     2                ICOCO
C
C      *****************************************************************
C
C***FIRST EXECUTABLE STATEMENT  DBVPOR
      NFCP1 = NFC + 1
      NUMORT = 0
      C = 1.0D0
C
C     ******************************************************************
C         CALCULATE INITIAL CONDITIONS WHICH SATISFY
C                       A*YH(XINITIAL)=0  AND  A*YP(XINITIAL)=ALPHA.
C         WHEN NFC .NE. NFCC DLSSUD DEFINES VALUES YHP IN A MATRIX OF
C         SIZE (NFCC+1)*NCOMP AND ,HENCE, OVERFLOWS THE STORAGE
C         ALLOCATION INTO THE U ARRAY. HOWEVER, THIS IS OKAY SINCE
C         PLENTY OF SPACE IS AVAILABLE IN U AND IT HAS NOT YET BEEN
C         USED.
C
      NDW = NROWA*NCOMP
      KWS = NDW + NIC + 1
      KWD = KWS + NIC
      KWT = KWD + NIC
      KWC = KWT + NIC
      IFLAG = 0
      CALL DLSSUD(A,YHP(1,NFCC+1),ALPHA,NIC,NCOMP,NROWA,YHP,NCOMP,IFLAG,
     1            1,IRA,0,WORK(1),WORK(NDW+1),IWORK,WORK(KWS),WORK(KWD),
     2            WORK(KWT),ISFLG,WORK(KWC))
      IF (IFLAG .EQ. 1) GO TO 10
         IFLAG = -4
      GO TO 200
   10 CONTINUE
         IF (NFC .NE. NFCC)
     1      CALL DVECS(NCOMP,NFC,YHP,WORK,IWORK,INHOMO,IFLAG)
         IF (IFLAG .EQ. 1) GO TO 20
            IFLAG = -5
         GO TO 190
   20    CONTINUE
C
C           ************************************************************
C               DETERMINE THE NUMBER OF DIFFERENTIAL EQUATIONS TO BE
C               INTEGRATED, INITIALIZE VARIABLES FOR AUXILIARY INITIAL
C               VALUE PROBLEM AND STORE INITIAL CONDITIONS.
C
            NEQ = NCOMP*NFC
            IF (INHOMO .EQ. 1) NEQ = NEQ + NCOMP
            IVP = 0
            IF (NEQIVP .EQ. 0) GO TO 40
               IVP = NEQ
               NEQ = NEQ + NEQIVP
               NFCP2 = NFCP1
               IF (INHOMO .EQ. 1) NFCP2 = NFCP1 + 1
               DO 30 K = 1, NEQIVP
                  YHP(K,NFCP2) = ALPHA(NIC+K)
   30          CONTINUE
   40       CONTINUE
            CALL DSTOR1(U,YHP,V,YHP(1,NFCP1),0,NDISK,NTAPE)
C
C           ************************************************************
C               SET UP DATA FOR THE ORTHONORMALIZATION TESTING PROCEDURE
C               AND SAVE INITIAL CONDITIONS IN CASE A RESTART IS
C               NECESSARY.
C
            NSWOT = 1
            KNSWOT = 0
            LOTJP = 1
            TND = LOG10(10.0D0*TOL)
            PWCND = LOG10(SQRT(TOL))
            X = XBEG
            PX = X
            XOT = XEND
            XOP = X
            KOP = 1
            CALL DSTWAY(U,V,YHP,0,STOWA)
C
C           ************************************************************
C           ******** FORWARD INTEGRATION OF ALL INITIAL VALUE EQUATIONS
C           **********
C           ************************************************************
C
            CALL DRKFAB(NCOMP,XPTS,NXPTS,NFC,IFLAG,Z,MXNON,P,NTP,IP,YHP,
     1                  NIV,U,V,W,S,STOWA,G,WORK,IWORK,NFCC)
            IF (IFLAG .NE. 0 .OR. ICOCO .EQ. 0) GO TO 180
C
C              *********************************************************
C              **************** BACKWARD SWEEP TO OBTAIN SOLUTION
C              *******************
C              *********************************************************
C
C                  CALCULATE SUPERPOSITION COEFFICIENTS AT XFINAL.
C
C                FOR THE DISK STORAGE VERSION, IT IS NOT NECESSARY TO
C                READ  U  AND  V AT THE LAST OUTPUT POINT, SINCE THE
C                LOCAL COPY OF EACH STILL EXISTS.
C
               KOD = 1
               IF (NDISK .EQ. 0) KOD = NXPTS
               I1 = 1 + NFCC*NFCC
               I2 = I1 + NFCC
               CALL DCOEF(U(1,1,KOD),V(1,KOD),NCOMP,NROWB,NFC,NIC,B,
     1                     BETA,COEF,INHOMO,RE,AE,WORK,WORK(I1),
     2                     WORK(I2),IWORK,IFLAG,NFCC)
C
C              *********************************************************
C                  CALCULATE SOLUTION AT OUTPUT POINTS BY RECURRING
C                  BACKWARDS.  AS WE RECUR BACKWARDS FROM XFINAL TO
C                  XINITIAL WE MUST CALCULATE NEW SUPERPOSITION
C                  COEFFICIENTS EACH TIME WE CROSS A POINT OF
C                  ORTHONORMALIZATION.
C
               K = NUMORT
               NCOMP2 = NCOMP/2
               IC = 1
               IF (NFC .NE. NFCC) IC = 2
               DO 170 J = 1, NXPTS
                  KPTS = NXPTS - J + 1
                  KOD = KPTS
                  IF (NDISK .EQ. 1) KOD = 1
   50             CONTINUE
C                 ...EXIT
                     IF (K .EQ. 0) GO TO 120
C                 ...EXIT
                     IF (XEND .GT. XBEG .AND. XPTS(KPTS) .GE. Z(K))
     1                  GO TO 120
C                 ...EXIT
                     IF (XEND .LT. XBEG .AND. XPTS(KPTS) .LE. Z(K))
     1                  GO TO 120
                     NON = K
                     IF (NDISK .EQ. 0) GO TO 60
                        NON = 1
                        BACKSPACE NTAPE
                        READ (NTAPE)
     1                       (IP(I,1), I = 1, NFCC),(P(I,1), I = 1, NTP)
                        BACKSPACE NTAPE
   60                CONTINUE
                     IF (INHOMO .NE. 1) GO TO 90
                        IF (NDISK .EQ. 0) GO TO 70
                           BACKSPACE NTAPE
                           READ (NTAPE) (W(I,1), I = 1, NFCC)
                           BACKSPACE NTAPE
   70                   CONTINUE
                        DO 80 N = 1, NFCC
                           COEF(N) = COEF(N) - W(N,NON)
   80                   CONTINUE
   90                CONTINUE
                     CALL DBKSOL(NFCC,P(1,NON),COEF)
                     DO 100 M = 1, NFCC
                        WORK(M) = COEF(M)
  100                CONTINUE
                     DO 110 M = 1, NFCC
                        L = IP(M,NON)
                        COEF(L) = WORK(M)
  110                CONTINUE
                     K = K - 1
                  GO TO 50
  120             CONTINUE
                  IF (NDISK .EQ. 0) GO TO 130
                     BACKSPACE NTAPE
                     READ (NTAPE)
     1                    (V(I,1), I = 1, NCOMP),
     2                    ((U(I,M,1), I = 1, NCOMP), M = 1, NFC)
                     BACKSPACE NTAPE
  130             CONTINUE
                  DO 140 N = 1, NCOMP
                     Y(N,KPTS) = V(N,KOD)
     1                           + DDOT(NFC,U(N,1,KOD),NCOMP,COEF,IC)
  140             CONTINUE
                  IF (NFC .EQ. NFCC) GO TO 160
                     DO 150 N = 1, NCOMP2
                        NN = NCOMP2 + N
                        Y(N,KPTS) = Y(N,KPTS)
     1                              - DDOT(NFC,U(NN,1,KOD),NCOMP,
     2                                     COEF(2),2)
                        Y(NN,KPTS) = Y(NN,KPTS)
     1                               + DDOT(NFC,U(N,1,KOD),NCOMP,
     2                                      COEF(2),2)
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
C
C     ******************************************************************
C
      MXNON = NUMORT
      RETURN
      END
