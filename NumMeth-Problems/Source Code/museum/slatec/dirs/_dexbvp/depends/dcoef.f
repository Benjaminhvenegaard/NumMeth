      SUBROUTINE DCOEF (YH, YP, NCOMP, NROWB, NFC, NIC, B, BETA, COEF,
     +   INHOMO, RE, AE, BY, CVEC, WORK, IWORK, IFLAG, NFCC)
C
      DOUBLE PRECISION DDOT
      INTEGER I, IFLAG, INHOMO, IWORK(*), J, K, KFLAG, KI, L, LPAR,
     1     MLSO, NCOMP, NCOMP2, NF, NFC, NFCC, NFCCM1, NIC,
     2     NROWB
      DOUBLE PRECISION AE, B(NROWB,*), BBN, BETA(*), BN, BRN,
     1     BY(NFCC,*), BYKL, BYS, COEF(*), CONS, CVEC(*), EPS,
     2     FOURU, GAM, RE, SQOVFL, SRU, TWOU, UN, URO, WORK(*),
     3     YH(NCOMP,*), YP(*), YPN
C
      COMMON /DML5MC/ URO,SRU,EPS,SQOVFL,TWOU,FOURU,LPAR
C***FIRST EXECUTABLE STATEMENT  DCOEF
C
C     SET UP MATRIX  B*YH  AND VECTOR  BETA - B*YP
C
      NCOMP2 = NCOMP/2
      DO 80 K = 1, NFCC
         DO 10 J = 1, NFC
            L = J
            IF (NFC .NE. NFCC) L = 2*J - 1
            BY(K,L) = DDOT(NCOMP,B(K,1),NROWB,YH(1,J),1)
   10    CONTINUE
         IF (NFC .EQ. NFCC) GO TO 30
            DO 20 J = 1, NFC
               L = 2*J
               BYKL = DDOT(NCOMP2,B(K,1),NROWB,YH(NCOMP2+1,J),1)
               BY(K,L) = DDOT(NCOMP2,B(K,NCOMP2+1),NROWB,YH(1,J),1)
     1                   - BYKL
   20       CONTINUE
   30    CONTINUE
         GO TO (40,50,60), INHOMO
C        CASE 1
   40    CONTINUE
            CVEC(K) = BETA(K) - DDOT(NCOMP,B(K,1),NROWB,YP,1)
         GO TO 70
C        CASE 2
   50    CONTINUE
            CVEC(K) = BETA(K)
         GO TO 70
C        CASE 3
   60    CONTINUE
            CVEC(K) = 0.0D0
   70    CONTINUE
   80 CONTINUE
      CONS = ABS(CVEC(1))
      BYS = ABS(BY(1,1))
C
C     ******************************************************************
C         SOLVE LINEAR SYSTEM
C
      IFLAG = 0
      MLSO = 0
      IF (INHOMO .EQ. 3) MLSO = 1
      KFLAG = 0.5D0 * LOG10(EPS)
      CALL XGETF(NF)
      CALL XSETF(0)
   90 CONTINUE
         CALL DSUDS(BY,COEF,CVEC,NFCC,NFCC,NFCC,KFLAG,MLSO,WORK,IWORK)
         IF (KFLAG .NE. 3) GO TO 100
         KFLAG = 1
         IFLAG = 1
      GO TO 90
  100 CONTINUE
      IF (KFLAG .EQ. 4) IFLAG = 2
      CALL XSETF(NF)
      IF (NFCC .EQ. 1) GO TO 180
         IF (INHOMO .NE. 3) GO TO 170
            IF (IWORK(1) .LT. NFCC) GO TO 140
               IFLAG = 3
               DO 110 K = 1, NFCC
                  COEF(K) = 0.0D0
  110          CONTINUE
               COEF(NFCC) = 1.0D0
               NFCCM1 = NFCC - 1
               DO 130 K = 1, NFCCM1
                  J = NFCC - K
                  L = NFCC - J + 1
                  GAM = DDOT(L,BY(J,J),NFCC,COEF(J),1)/(WORK(J)*BY(J,J))
                  DO 120 I = J, NFCC
                     COEF(I) = COEF(I) + GAM*BY(J,I)
  120             CONTINUE
  130          CONTINUE
            GO TO 160
  140       CONTINUE
               DO 150 K = 1, NFCC
                  KI = 4*NFCC + K
                  COEF(K) = WORK(KI)
  150          CONTINUE
  160       CONTINUE
  170    CONTINUE
      GO TO 220
  180 CONTINUE
C
C        ***************************************************************
C            TESTING FOR EXISTENCE AND UNIQUENESS OF BOUNDARY-VALUE
C            PROBLEM SOLUTION IN A SCALAR CASE
C
         BN = 0.0D0
         UN = 0.0D0
         YPN = 0.0D0
         DO 190 K = 1, NCOMP
            UN = MAX(UN,ABS(YH(K,1)))
            YPN = MAX(YPN,ABS(YP(K)))
            BN = MAX(BN,ABS(B(1,K)))
  190    CONTINUE
         BBN = MAX(BN,ABS(BETA(1)))
         IF (BYS .GT. 10.0D0*(RE*UN + AE)*BN) GO TO 200
            BRN = BBN/BN*BYS
            IF (CONS .GE. 0.1D0*BRN .AND. CONS .LE. 10.0D0*BRN)
     1         IFLAG = 1
            IF (CONS .GT. 10.0D0*BRN) IFLAG = 2
            IF (CONS .LE. RE*ABS(BETA(1)) + AE + (RE*YPN + AE)*BN)
     1         IFLAG = 1
            IF (INHOMO .EQ. 3) COEF(1) = 1.0D0
         GO TO 210
  200    CONTINUE
         IF (INHOMO .NE. 3) GO TO 210
            IFLAG = 3
            COEF(1) = 1.0D0
  210    CONTINUE
  220 CONTINUE
      RETURN
      END
