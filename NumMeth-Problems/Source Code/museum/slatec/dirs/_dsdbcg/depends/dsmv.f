      SUBROUTINE DSMV (N, X, Y, NELT, IA, JA, A, ISYM)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), X(N), Y(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, IBGN, ICOL, IEND, IROW, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  DSMV
C
C         Zero out the result vector.
C
      DO 10 I = 1, N
         Y(I) = 0
 10   CONTINUE
C
C         Multiply by A.
C
CVD$R NOCONCUR
      DO 30 ICOL = 1, N
         IBGN = JA(ICOL)
         IEND = JA(ICOL+1)-1
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
         DO 20 I = IBGN, IEND
            Y(IA(I)) = Y(IA(I)) + A(I)*X(ICOL)
 20      CONTINUE
 30   CONTINUE
C
      IF( ISYM.EQ.1 ) THEN
C
C         The matrix is non-symmetric.  Need to get the other half in...
C         This loops assumes that the diagonal is the first entry in
C         each column.
C
         DO 50 IROW = 1, N
            JBGN = JA(IROW)+1
            JEND = JA(IROW+1)-1
            IF( JBGN.GT.JEND ) GOTO 50
            DO 40 J = JBGN, JEND
               Y(IROW) = Y(IROW) + A(J)*X(IA(J))
 40         CONTINUE
 50      CONTINUE
      ENDIF
      RETURN
C------------- LAST LINE OF DSMV FOLLOWS ----------------------------
      END
