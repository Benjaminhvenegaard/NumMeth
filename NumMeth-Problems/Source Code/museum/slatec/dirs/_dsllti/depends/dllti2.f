      SUBROUTINE DLLTI2 (N, B, X, NEL, IEL, JEL, EL, DINV)
C     .. Scalar Arguments ..
      INTEGER N, NEL
C     .. Array Arguments ..
      DOUBLE PRECISION B(N), DINV(N), EL(NEL), X(N)
      INTEGER IEL(NEL), JEL(NEL)
C     .. Local Scalars ..
      INTEGER I, IBGN, IEND, IROW
C***FIRST EXECUTABLE STATEMENT  DLLTI2
C
C         Solve  L*y = b,  storing result in x.
C
      DO 10 I=1,N
         X(I) = B(I)
 10   CONTINUE
      DO 30 IROW = 1, N
         IBGN = IEL(IROW) + 1
         IEND = IEL(IROW+1) - 1
         IF( IBGN.LE.IEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NOCONCUR
CVD$ NODEPCHK
            DO 20 I = IBGN, IEND
               X(IROW) = X(IROW) - EL(I)*X(JEL(I))
 20         CONTINUE
         ENDIF
 30   CONTINUE
C
C         Solve  D*Z = Y,  storing result in X.
C
      DO 40 I=1,N
         X(I) = X(I)*DINV(I)
 40   CONTINUE
C
C         Solve  L-trans*X = Z.
C
      DO 60 IROW = N, 2, -1
         IBGN = IEL(IROW) + 1
         IEND = IEL(IROW+1) - 1
         IF( IBGN.LE.IEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NOCONCUR
CVD$ NODEPCHK
            DO 50 I = IBGN, IEND
               X(JEL(I)) = X(JEL(I)) - EL(I)*X(IROW)
 50         CONTINUE
         ENDIF
 60   CONTINUE
C
      RETURN
C------------- LAST LINE OF DLLTI2 FOLLOWS ----------------------------
      END
