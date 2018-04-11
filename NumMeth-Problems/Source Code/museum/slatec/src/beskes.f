      SUBROUTINE BESKES (XNU, X, NIN, BKE)
      DIMENSION BKE(*)
      SAVE ALNBIG
      DATA ALNBIG / 0. /
C***FIRST EXECUTABLE STATEMENT  BESKES
      IF (ALNBIG.EQ.0.) ALNBIG = LOG (R1MACH(2))
C
      V = ABS(XNU)
      N = ABS(NIN)
C
      IF (V .GE. 1.) CALL XERMSG ('SLATEC', 'BESKES',
     +   'ABS(XNU) MUST BE LT 1', 2, 2)
      IF (X .LE. 0.) CALL XERMSG ('SLATEC', 'BESKES', 'X IS LE 0', 3,
     +   2)
      IF (N .EQ. 0) CALL XERMSG ('SLATEC', 'BESKES',
     +   'N THE NUMBER IN THE SEQUENCE IS 0', 4, 2)
C
      CALL R9KNUS (V, X, BKE(1), BKNU1, ISWTCH)
      IF (N.EQ.1) RETURN
C
      VINCR = SIGN (1.0, REAL(NIN))
      DIRECT = VINCR
      IF (XNU.NE.0.) DIRECT = VINCR*SIGN(1.0,XNU)
      IF (ISWTCH .EQ. 1 .AND. DIRECT .GT. 0.) CALL XERMSG ('SLATEC',
     +   'BESKES', 'X SO SMALL BESSEL K-SUB-XNU+1 OVERFLOWS', 5, 2)
      BKE(2) = BKNU1
C
      IF (DIRECT.LT.0.) CALL R9KNUS (ABS(XNU+VINCR), X, BKE(2), BKNU1,
     1  ISWTCH)
      IF (N.EQ.2) RETURN
C
      VEND = ABS(XNU+NIN) - 1.0
      IF ((VEND-0.5)*LOG(VEND)+0.27-VEND*(LOG(X)-.694) .GT. ALNBIG)
     1CALL XERMSG ( 'SLATEC', 'BESKES',
     2'X SO SMALL OR ABS(NU) SO BIG THAT BESSEL K-SUB-NU OVERFLOWS',
     35, 2)
C
      V = XNU
      DO 10 I=3,N
        V = V + VINCR
        BKE(I) = 2.0*V*BKE(I-1)/X + BKE(I-2)
 10   CONTINUE
C
      RETURN
      END