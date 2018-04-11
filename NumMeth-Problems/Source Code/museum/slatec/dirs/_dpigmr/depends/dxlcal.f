      SUBROUTINE DXLCAL (N, LGMR, X, XL, ZL, HES, MAXLP1, Q, V, R0NRM,
     +   WK, SZ, JSCAL, JPRE, MSOLVE, NMSL, RPAR, IPAR, NELT, IA, JA, A,
     +   ISYM)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      DOUBLE PRECISION R0NRM
      INTEGER ISYM, JPRE, JSCAL, LGMR, MAXLP1, N, NELT, NMSL
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), HES(MAXLP1,*), Q(*), RPAR(*), SZ(*),
     +                 V(N,*), WK(N), X(N), XL(N), ZL(N)
      INTEGER IA(NELT), IPAR(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MSOLVE
C     .. Local Scalars ..
      INTEGER I, K, LL, LLP1
C     .. External Subroutines ..
      EXTERNAL DAXPY, DCOPY, DHELS
C***FIRST EXECUTABLE STATEMENT  DXLCAL
      LL = LGMR
      LLP1 = LL + 1
      DO 10 K = 1,LLP1
         WK(K) = 0
 10   CONTINUE
      WK(1) = R0NRM
      CALL DHELS(HES, MAXLP1, LL, Q, WK)
      DO 20 K = 1,N
         ZL(K) = 0
 20   CONTINUE
      DO 30 I = 1,LL
         CALL DAXPY(N, WK(I), V(1,I), 1, ZL, 1)
 30   CONTINUE
      IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
         DO 40 K = 1,N
            ZL(K) = ZL(K)/SZ(K)
 40      CONTINUE
      ENDIF
      IF (JPRE .GT. 0) THEN
         CALL DCOPY(N, ZL, 1, WK, 1)
         CALL MSOLVE(N, WK, ZL, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      ENDIF
C         calculate XL from X and ZL.
      DO 50 K = 1,N
         XL(K) = X(K) + ZL(K)
 50   CONTINUE
      RETURN
C------------- LAST LINE OF DXLCAL FOLLOWS ----------------------------
      END
