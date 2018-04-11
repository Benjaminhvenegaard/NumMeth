      SUBROUTINE SSLUI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCIL, LOCIU, LOCJL, LOCJU, LOCL, LOCU
C     .. External Subroutines ..
      EXTERNAL SSLUI2
C***FIRST EXECUTABLE STATEMENT  SSLUI
C
C         Pull out the locations of the arrays holding the ILU
C         factorization.
C
      LOCIL = IWORK(1)
      LOCJL = IWORK(2)
      LOCIU = IWORK(3)
      LOCJU = IWORK(4)
      LOCL = IWORK(5)
      LOCDIN = IWORK(6)
      LOCU = IWORK(7)
C
C         Solve the system LUx = b
      CALL SSLUI2(N, B, X, IWORK(LOCIL), IWORK(LOCJL), RWORK(LOCL),
     $     RWORK(LOCDIN), IWORK(LOCIU), IWORK(LOCJU), RWORK(LOCU) )
C
      RETURN
C------------- LAST LINE OF SSLUI FOLLOWS ----------------------------
      END
