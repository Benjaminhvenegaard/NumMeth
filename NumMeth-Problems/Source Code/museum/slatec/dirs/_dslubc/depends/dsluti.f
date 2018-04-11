      SUBROUTINE DSLUTI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(N), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCIL, LOCIU, LOCJL, LOCJU, LOCL, LOCU
C     .. External Subroutines ..
      EXTERNAL DSLUI4
C***FIRST EXECUTABLE STATEMENT  DSLUTI
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
      CALL DSLUI4(N, B, X, IWORK(LOCIL), IWORK(LOCJL), RWORK(LOCL),
     $     RWORK(LOCDIN), IWORK(LOCIU), IWORK(LOCJU), RWORK(LOCU))
C
      RETURN
C------------- LAST LINE OF DSLUTI FOLLOWS ----------------------------
      END
