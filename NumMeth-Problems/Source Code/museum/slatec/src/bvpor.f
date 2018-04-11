      SUBROUTINE BVPOR (Y, NROWY, NCOMP, XPTS, NXPTS, A, NROWA, ALPHA,
     +   NIC, B, NROWB, BETA, NFC, IFLAG, Z, MXNON, P, NTP, IP, W, NIV,
     +   YHP, U, V, COEF, S, STOWA, G, WORK, IWORK, NFCC)
C
      DIMENSION Y(NROWY,*),A(NROWA,*),ALPHA(*),B(NROWB,*),
     1          BETA(*),P(NTP,*),IP(NFCC,*),
     2          U(NCOMP,NFC,*),V(NCOMP,*),W(NFCC,*),
     3          COEF(*),Z(*),YHP(NCOMP,*),XPTS(*),S(*),
     4          WORK(*),IWORK(*),STOWA(*),G(*)
C
C **********************************************************************
C
      COMMON /ML8SZ/ C,XSAV,IGOFX,INHOMO,IVP,NCOMPD,NFCD
      COMMON /ML15TO/ PX,PWCND,TND,X,XBEG,XEND,XOT,XOP,INFO(15),ISTKOP,
     1                KNSWOT,KOP,LOTJP,MNSWOT,NSWOT
      COMMON /ML18JR/ AE,RE,TOL,NXPTSD,NICD,NOPG,MXNOND,NDISK,NTAPE,
     1                NEQ,INDPVT,INTEG,NPS,NTPD,NEQIVP,NUMORT,NFCCD,
     2                ICOCO
C
C **********************************************************************
C
C***FIRST EXECUTABLE STATEMENT  BVPOR
      NFCP1 = NFC + 1
      NUMORT = 0
      C = 1.0
C
C **********************************************************************
C     CALCULATE INITIAL CONDITIONS WHICH SATISFY
C                   A*YH(XINITIAL)=0  AND  A*YP(XINITIAL)=ALPHA.
C     WHEN NFC .NE. NFCC LSSUDS DEFINES VALUES YHP IN A MATRIX OF SIZE
C     (NFCC+1)*NCOMP AND ,HENCE, OVERFLOWS THE STORAGE ALLOCATION INTO
C     THE U ARRAY. HOWEVER, THIS IS OKAY SINCE PLENTY OF SPACE IS
C     AVAILABLE IN U AND IT HAS NOT YET BEEN USED.
C
      NDW = NROWA * NCOMP
      KWS = NDW + NIC + 1
      KWD = KWS + NIC
      KWT = KWD + NIC
      KWC = KWT + NIC
      IFLAG = 0
      CALL LSSUDS(A,YHP(1,NFCC+1),ALPHA,NIC,NCOMP,NROWA,YHP,NCOMP,
     1            IFLAG,1,IRA,0,WORK(1),WORK(NDW+1),IWORK,WORK(KWS),
     2            WORK(KWD),WORK(KWT),ISFLG,WORK(KWC))
      IF (IFLAG .EQ. 1) GO TO 3
      IFLAG=-4
      GO TO 250
    3 IF (NFC .NE. NFCC) CALL SVECS(NCOMP,NFC,YHP,WORK,IWORK,
     1                   INHOMO,IFLAG)
      IF (IFLAG .EQ. 1)  GO TO 5
      IFLAG=-5
      GO TO 250
C
C **********************************************************************
C     DETERMINE THE NUMBER OF DIFFERENTIAL EQUATIONS TO BE INTEGRATED,
C     INITIALIZE VARIABLES FOR AUXILIARY INITIAL VALUE PROBLEM AND
C     STORE INITIAL CONDITIONS.
C
    5 NEQ = NCOMP * NFC
      IF (INHOMO .EQ. 1)  NEQ = NEQ + NCOMP
      IVP = 0
      IF (NEQIVP .EQ. 0)  GO TO 10
      IVP = NEQ
      NEQ = NEQ + NEQIVP
      NFCP2 = NFCP1
      IF (INHOMO .EQ. 1)  NFCP2 = NFCP1 + 1
      DO 7 K = 1,NEQIVP
    7 YHP(K,NFCP2) = ALPHA(NIC+K)
   10 CALL STOR1(U,YHP,V,YHP(1,NFCP1),0,NDISK,NTAPE)
C
C **********************************************************************
C     SET UP DATA FOR THE ORTHONORMALIZATION TESTING PROCEDURE AND
C     SAVE INITIAL CONDITIONS IN CASE A RESTART IS NECESSARY.
C
      NSWOT=1
      KNSWOT=0
      LOTJP=1
      TND=LOG10(10.*TOL)
      PWCND=LOG10(SQRT(TOL))
      X=XBEG
      PX=X
      XOT=XEND
      XOP=X
      KOP=1
      CALL STWAY(U,V,YHP,0,STOWA)
C
C **********************************************************************
C ******** FORWARD INTEGRATION OF ALL INITIAL VALUE EQUATIONS **********
C **********************************************************************
C
      CALL RKFAB(NCOMP,XPTS,NXPTS,NFC,IFLAG,Z,MXNON,P,NTP,IP,
     1            YHP,NIV,U,V,W,S,STOWA,G,WORK,IWORK,NFCC)
      IF (IFLAG .NE. 0  .OR.  ICOCO .EQ. 0)  GO TO 250
C
C **********************************************************************
C **************** BACKWARD SWEEP TO OBTAIN SOLUTION *******************
C **********************************************************************
C
C     CALCULATE SUPERPOSITION COEFFICIENTS AT XFINAL.
C
C   FOR THE DISK STORAGE VERSION, IT IS NOT NECESSARY TO READ  U  AND  V
C   AT THE LAST OUTPUT POINT, SINCE THE LOCAL COPY OF EACH STILL EXISTS.
C
      KOD = 1
      IF (NDISK .EQ. 0)  KOD = NXPTS
      I1=1+NFCC*NFCC
      I2=I1+NFCC
      CALL SCOEF(U(1,1,KOD),V(1,KOD),NCOMP,NROWB,NFC,NIC,B,BETA,COEF,
     1           INHOMO,RE,AE,WORK,WORK(I1),WORK(I2),IWORK,IFLAG,NFCC)
C
C **********************************************************************
C     CALCULATE SOLUTION AT OUTPUT POINTS BY RECURRING BACKWARDS.
C     AS WE RECUR BACKWARDS FROM XFINAL TO XINITIAL WE MUST CALCULATE
C     NEW SUPERPOSITION COEFFICIENTS EACH TIME WE CROSS A POINT OF
C     ORTHONORMALIZATION.
C
      K = NUMORT
      NCOMP2=NCOMP/2
      IC=1
      IF (NFC .NE. NFCC) IC=2
      DO 200 J = 1,NXPTS
      KPTS = NXPTS - J + 1
      KOD = KPTS
      IF (NDISK .EQ. 1)  KOD = 1
  135 IF (K .EQ. 0)  GO TO 170
      IF (XEND.GT.XBEG .AND. XPTS(KPTS).GE.Z(K))  GO TO 170
      IF (XEND.LT.XBEG .AND. XPTS(KPTS).LE.Z(K))  GO TO 170
      NON = K
      IF (NDISK .EQ. 0)  GO TO 136
      NON = 1
      BACKSPACE NTAPE
      READ (NTAPE) (IP(I,1), I = 1,NFCC),(P(I,1), I = 1,NTP)
      BACKSPACE NTAPE
  136 IF (INHOMO .NE. 1)  GO TO 150
      IF (NDISK .EQ. 0)  GO TO 138
      BACKSPACE NTAPE
      READ (NTAPE) (W(I,1), I = 1,NFCC)
      BACKSPACE NTAPE
  138 DO 140 N = 1,NFCC
  140 COEF(N) = COEF(N) - W(N,NON)
  150 CALL BKSOL(NFCC,P(1,NON),COEF)
      DO 155 M = 1,NFCC
  155 WORK(M) = COEF(M)
      DO 160 M = 1,NFCC
      L = IP(M,NON)
  160 COEF(L) = WORK(M)
      K = K - 1
      GO TO 135
  170 IF (NDISK .EQ. 0)  GO TO 175
      BACKSPACE NTAPE
      READ (NTAPE) (V(I,1), I = 1,NCOMP),
     1             ((U(I,M,1), I = 1,NCOMP), M = 1,NFC)
      BACKSPACE NTAPE
  175 DO 180 N = 1,NCOMP
  180 Y(N,KPTS) = V(N,KOD) + SDOT(NFC,U(N,1,KOD),NCOMP,COEF,IC)
      IF (NFC .EQ. NFCC) GO TO 200
      DO 190 N=1,NCOMP2
      NN=NCOMP2+N
      Y(N,KPTS)=Y(N,KPTS) - SDOT(NFC,U(NN,1,KOD),NCOMP,COEF(2),2)
  190 Y(NN,KPTS)=Y(NN,KPTS) + SDOT(NFC,U(N,1,KOD),NCOMP,COEF(2),2)
  200 CONTINUE
C
C **********************************************************************
C
  250 MXNON = NUMORT
      RETURN
      END
