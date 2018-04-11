      SUBROUTINE DSLLTI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(*), RWORK(*), X(*)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCEL, LOCIEL, LOCJEL, NEL
C     .. External Subroutines ..
      EXTERNAL DLLTI2
C***FIRST EXECUTABLE STATEMENT  DSLLTI
      NEL = IWORK(1)
      LOCIEL = IWORK(3)
      LOCJEL = IWORK(2)
      LOCEL  = IWORK(4)
      LOCDIN = IWORK(5)
      CALL DLLTI2(N, B, X, NEL, IWORK(LOCIEL), IWORK(LOCJEL),
     $     RWORK(LOCEL), RWORK(LOCDIN))
C
      RETURN
C------------- LAST LINE OF DSLLTI FOLLOWS ----------------------------
      END
