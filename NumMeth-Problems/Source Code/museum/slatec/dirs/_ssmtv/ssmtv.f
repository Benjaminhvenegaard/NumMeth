      SUBROUTINE SSMTV (N, X, Y, NELT, IA, JA, A, ISYM)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), X(N), Y(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, IBGN, ICOL, IEND, IROW, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  SSMTV
C
C         Zero out the result vector.
C
      DO 10 I = 1, N
         Y(I) = 0
 10   CONTINUE
C
C         Multiply by A-Transpose.
C         A-Transpose is stored by rows...
CVD$R NOCONCUR
      DO 30 IROW = 1, N
         IBGN = JA(IROW)
         IEND = JA(IROW+1)-1
CVD$ ASSOC
         DO 20 I = IBGN, IEND
            Y(IROW) = Y(IROW) + A(I)*X(IA(I))
 20      CONTINUE
 30   CONTINUE
C
      IF( ISYM.EQ.1 ) THEN
C
C         The matrix is non-symmetric.  Need to get the other half in...
C         This loops assumes that the diagonal is the first entry in
C         each column.
C
         DO 50 ICOL = 1, N
            JBGN = JA(ICOL)+1
            JEND = JA(ICOL+1)-1
            IF( JBGN.GT.JEND ) GOTO 50
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
            DO 40 J = JBGN, JEND
               Y(IA(J)) = Y(IA(J)) + A(J)*X(ICOL)
 40         CONTINUE
 50      CONTINUE
      ENDIF
      RETURN
C------------- LAST LINE OF SSMTV FOLLOWS ----------------------------
      END
