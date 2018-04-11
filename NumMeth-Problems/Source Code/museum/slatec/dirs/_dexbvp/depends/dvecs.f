      SUBROUTINE DVECS (NCOMP, LNFC, YHP, WORK, IWORK, INHOMO, IFLAG)
C
      INTEGER ICOCO, IDP, IFLAG, INDPVT, INHOMO, INTEG, IWORK(*), K,
     1     KP, LNFC, LNFCC, MXNON, NCOMP, NDISK, NEQ, NEQIVP, NIC, NIV,
     2     NOPG, NPS, NTAPE, NTP, NUMORT, NXPTS
      DOUBLE PRECISION AE, DUM, RE, TOL, WORK(*), YHP(NCOMP,*)
      COMMON /DML18J/ AE,RE,TOL,NXPTS,NIC,NOPG,MXNON,NDISK,NTAPE,NEQ,
     1                INDPVT,INTEG,NPS,NTP,NEQIVP,NUMORT,LNFCC,
     2                ICOCO
C***FIRST EXECUTABLE STATEMENT  DVECS
         IF (LNFC .NE. 1) GO TO 20
            DO 10 K = 1, NCOMP
               YHP(K,LNFC+1) = YHP(K,LNFCC+1)
   10       CONTINUE
            IFLAG = 1
         GO TO 60
   20    CONTINUE
            NIV = LNFC
            LNFC = 2*LNFC
            LNFCC = 2*LNFCC
            KP = LNFC + 2 + LNFCC
            IDP = INDPVT
            INDPVT = 0
            CALL DMGSBV(NCOMP,LNFC,YHP,NCOMP,NIV,IFLAG,WORK(1),WORK(KP),
     1                  IWORK(1),INHOMO,YHP(1,LNFC+1),WORK(LNFC+2),DUM)
            LNFC = LNFC/2
            LNFCC = LNFCC/2
            INDPVT = IDP
            IF (IFLAG .NE. 0 .OR. NIV .NE. LNFC) GO TO 40
               DO 30 K = 1, NCOMP
                  YHP(K,LNFC+1) = YHP(K,LNFCC+1)
   30          CONTINUE
               IFLAG = 1
            GO TO 50
   40       CONTINUE
               IFLAG = 99
   50       CONTINUE
   60    CONTINUE
      CONTINUE
      RETURN
      END
