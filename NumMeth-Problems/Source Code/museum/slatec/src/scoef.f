      SUBROUTINE SCOEF (YH, YP, NCOMP, NROWB, NFC, NIC, B, BETA, COEF,
     +   INHOMO, RE, AE, BY, CVEC, WORK, IWORK, IFLAG, NFCC)
C
      DIMENSION YH(NCOMP,*),YP(*),B(NROWB,*),BETA(*),
     1          COEF(*),BY(NFCC,*),CVEC(*),WORK(*),IWORK(*)
C
      COMMON /ML5MCO/ URO,SRU,EPS,SQOVFL,TWOU,FOURU,LPAR
C
C     SET UP MATRIX  B*YH  AND VECTOR  BETA - B*YP
C
C***FIRST EXECUTABLE STATEMENT  SCOEF
      NCOMP2=NCOMP/2
      DO 7 K = 1,NFCC
      DO 1 J = 1,NFC
      L=J
      IF (NFC .NE. NFCC) L=2*J-1
    1 BY(K,L) = SDOT(NCOMP,B(K,1),NROWB,YH(1,J),1)
      IF (NFC .EQ. NFCC) GO TO 3
      DO 2 J=1,NFC
      L=2*J
      BYKL=SDOT(NCOMP2,B(K,1),NROWB,YH(NCOMP2+1,J),1)
      BY(K,L)=SDOT(NCOMP2,B(K,NCOMP2+1),NROWB,YH(1,J),1) - BYKL
    2 CONTINUE
    3 GO TO (4,5,6), INHOMO
C     CASE 1
    4 CVEC(K) = BETA(K) - SDOT(NCOMP,B(K,1),NROWB,YP,1)
      GO TO 7
C     CASE 2
    5 CVEC(K) = BETA(K)
      GO TO 7
C     CASE 3
    6 CVEC(K) = 0.
    7 CONTINUE
      CONS=ABS(CVEC(1))
      BYS=ABS(BY(1,1))
C
C **********************************************************************
C     SOLVE LINEAR SYSTEM
C
      IFLAG=0
      MLSO=0
      IF (INHOMO .EQ. 3) MLSO=1
      KFLAG = 0.5 * LOG10(EPS)
      CALL XGETF(NF)
      CALL XSETF(0)
   10 CALL SUDS(BY,COEF,CVEC,NFCC,NFCC,NFCC,KFLAG,MLSO,WORK,IWORK)
      IF (KFLAG .NE. 3) GO TO 13
      KFLAG=1
      IFLAG=1
      GO TO 10
   13 IF (KFLAG .EQ. 4) IFLAG=2
      CALL XSETF(NF)
      IF (NFCC .EQ. 1) GO TO 25
      IF (INHOMO .NE. 3) RETURN
      IF (IWORK(1) .LT. NFCC) GO TO 17
      IFLAG=3
      DO 14 K=1,NFCC
   14 COEF(K)=0.
      COEF(NFCC)=1.
      NFCCM1=NFCC-1
      DO 15 K=1,NFCCM1
      J=NFCC-K
      L=NFCC-J+1
      GAM=SDOT(L,BY(J,J),NFCC,COEF(J),1)/(WORK(J)*BY(J,J))
      DO 15 I=J,NFCC
   15 COEF(I)=COEF(I)+GAM*BY(J,I)
      RETURN
   17 DO 20 K=1,NFCC
      KI=4*NFCC+K
   20 COEF(K)=WORK(KI)
      RETURN
C
C **********************************************************************
C     TESTING FOR EXISTENCE AND UNIQUENESS OF BOUNDARY-VALUE PROBLEM
C     SOLUTION IN A SCALAR CASE
C
   25 BN = 0.
      UN = 0.
      YPN=0.
      DO 30 K = 1,NCOMP
      UN = MAX(UN,ABS(YH(K,1)))
      YPN=MAX(YPN,ABS(YP(K)))
   30 BN = MAX(BN,ABS(B(1,K)))
      BBN = MAX(BN,ABS(BETA(1)))
      IF (BYS .GT. 10.*(RE*UN + AE)*BN)  GO TO 35
      BRN = BBN / BN * BYS
      IF (CONS .GE. 0.1*BRN  .AND.  CONS .LE. 10.*BRN) IFLAG=1
      IF (CONS .GT. 10.*BRN) IFLAG=2
      IF (CONS  .LE.  RE*ABS(BETA(1))+AE + (RE*YPN+AE)*BN) IFLAG=1
      IF (INHOMO .EQ. 3) COEF(1)=1.
      RETURN
   35 IF (INHOMO .NE. 3) RETURN
      IFLAG=3
      COEF(1)=1.
      RETURN
      END
