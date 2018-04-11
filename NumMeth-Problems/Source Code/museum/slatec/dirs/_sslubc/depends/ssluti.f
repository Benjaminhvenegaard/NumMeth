      SUBROUTINE SSLUTI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      REAL A(N), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCIL, LOCIU, LOCJL, LOCJU, LOCL, LOCU
C     .. External Subroutines ..
      EXTERNAL SSLUI4
C***FIRST EXECUTABLE STATEMENT  SSLUTI
C
C         Pull out the pointers to the L, D and U matrices and call
C         the workhorse routine.
C
      LOCIL = IWORK(1)
      LOCJL = IWORK(2)
      LOCIU = IWORK(3)
      LOCJU = IWORK(4)
      LOCL = IWORK(5)
      LOCDIN = IWORK(6)
      LOCU = IWORK(7)
C
      CALL SSLUI4(N, B, X, IWORK(LOCIL), IWORK(LOCJL), RWORK(LOCL),
     $     RWORK(LOCDIN), IWORK(LOCIU), IWORK(LOCJU), RWORK(LOCU))
C
      RETURN
C------------- LAST LINE OF SSLUTI FOLLOWS ----------------------------
      END
