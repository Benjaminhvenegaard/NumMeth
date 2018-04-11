      SUBROUTINE DSLI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCEL, LOCIEL, LOCJEL, NEL
C     .. External Subroutines ..
      EXTERNAL DSLI2
C***FIRST EXECUTABLE STATEMENT  DSLI
C
      NEL = IWORK(1)
      LOCIEL = IWORK(2)
      LOCJEL = IWORK(3)
      LOCEL = IWORK(4)
      CALL DSLI2(N, B, X, NEL, IWORK(LOCIEL), IWORK(LOCJEL),
     $     RWORK(LOCEL))
C
      RETURN
C------------- LAST LINE OF DSLI FOLLOWS ----------------------------
      END
