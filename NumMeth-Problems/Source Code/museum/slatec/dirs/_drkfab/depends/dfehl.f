      SUBROUTINE DFEHL (DF, NEQ, T, Y, H, YP, F1, F2, F3, F4, F5, YS,
     +   RPAR, IPAR)
C
      INTEGER IPAR, K, NEQ
      DOUBLE PRECISION CH, F1, F2, F3, F4, F5, H, RPAR, T, Y, YP, YS
      DIMENSION Y(*),YP(*),F1(*),F2(*),F3(*),F4(*),F5(*),
     1          YS(*),RPAR(*),IPAR(*)
C
C***FIRST EXECUTABLE STATEMENT  DFEHL
      CH = H/4.0D0
      DO 10 K = 1, NEQ
         YS(K) = Y(K) + CH*YP(K)
   10 CONTINUE
      CALL DF(T+CH,YS,F1,RPAR,IPAR)
C
      CH = 3.0D0*H/32.0D0
      DO 20 K = 1, NEQ
         YS(K) = Y(K) + CH*(YP(K) + 3.0D0*F1(K))
   20 CONTINUE
      CALL DF(T+3.0D0*H/8.0D0,YS,F2,RPAR,IPAR)
C
      CH = H/2197.0D0
      DO 30 K = 1, NEQ
         YS(K) = Y(K)
     1           + CH
     2             *(1932.0D0*YP(K) + (7296.0D0*F2(K) - 7200.0D0*F1(K)))
   30 CONTINUE
      CALL DF(T+12.0D0*H/13.0D0,YS,F3,RPAR,IPAR)
C
      CH = H/4104.0D0
      DO 40 K = 1, NEQ
         YS(K) = Y(K)
     1           + CH
     2             *((8341.0D0*YP(K) - 845.0D0*F3(K))
     3               + (29440.0D0*F2(K) - 32832.0D0*F1(K)))
   40 CONTINUE
      CALL DF(T+H,YS,F4,RPAR,IPAR)
C
      CH = H/20520.0D0
      DO 50 K = 1, NEQ
         YS(K) = Y(K)
     1           + CH
     2             *((-6080.0D0*YP(K)
     3                + (9295.0D0*F3(K) - 5643.0D0*F4(K)))
     4               + (41040.0D0*F1(K) - 28352.0D0*F2(K)))
   50 CONTINUE
      CALL DF(T+H/2.0D0,YS,F5,RPAR,IPAR)
C
C     COMPUTE APPROXIMATE SOLUTION AT T+H
C
      CH = H/7618050.0D0
      DO 60 K = 1, NEQ
         YS(K) = Y(K)
     1           + CH
     2             *((902880.0D0*YP(K)
     3                + (3855735.0D0*F3(K) - 1371249.0D0*F4(K)))
     4               + (3953664.0D0*F2(K) + 277020.0D0*F5(K)))
   60 CONTINUE
C
      RETURN
      END
