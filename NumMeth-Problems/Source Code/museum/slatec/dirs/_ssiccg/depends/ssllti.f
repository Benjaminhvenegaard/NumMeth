      SUBROUTINE SSLLTI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(*), RWORK(*), X(*)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCEL, LOCIEL, LOCJEL, NEL
C     .. External Subroutines ..
      EXTERNAL SLLTI2
C***FIRST EXECUTABLE STATEMENT  SSLLTI
      NEL = IWORK(1)
      LOCIEL = IWORK(3)
      LOCJEL = IWORK(2)
      LOCEL  = IWORK(4)
      LOCDIN = IWORK(5)
      CALL SLLTI2(N, B, X, NEL, IWORK(LOCIEL), IWORK(LOCJEL),
     $     RWORK(LOCEL), RWORK(LOCDIN))
C
      RETURN
C------------- LAST LINE OF SSLLTI FOLLOWS ----------------------------
      END
