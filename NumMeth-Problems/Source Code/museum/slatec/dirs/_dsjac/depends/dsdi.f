      SUBROUTINE DSDI (N, B, X, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, LOCD
C***FIRST EXECUTABLE STATEMENT  DSDI
C
C         Determine where the inverse of the diagonal
C         is in the work array and then scale by it.
C
      LOCD = IWORK(4) - 1
      DO 10 I = 1, N
         X(I) = RWORK(LOCD+I)*B(I)
 10   CONTINUE
      RETURN
C------------- LAST LINE OF DSDI FOLLOWS ----------------------------
      END
