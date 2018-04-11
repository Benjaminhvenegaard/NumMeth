      SUBROUTINE DSD2S (N, NELT, IA, JA, A, ISYM, DINV)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), DINV(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, K, KBGN, KEND
C***FIRST EXECUTABLE STATEMENT  DSD2S
      DO 10 I = 1, N
         DINV(I) = 0
 10   CONTINUE
C
C         Loop over each column.
CVD$R NOCONCUR
      DO 40 I = 1, N
         KBGN = JA(I)
         KEND = JA(I+1) - 1
C
C         Add in the contributions for each row that has a non-zero
C         in this column.
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
         DO 20 K = KBGN, KEND
            DINV(IA(K)) = DINV(IA(K)) + A(K)**2
 20      CONTINUE
         IF( ISYM.EQ.1 ) THEN
C
C         Lower triangle stored by columns => upper triangle stored by
C         rows with Diagonal being the first entry.  Loop across the
C         rest of the row.
            KBGN = KBGN + 1
            IF( KBGN.LE.KEND ) THEN
               DO 30 K = KBGN, KEND
                  DINV(I) = DINV(I) + A(K)**2
 30            CONTINUE
            ENDIF
         ENDIF
 40   CONTINUE
      DO 50 I=1,N
         DINV(I) = 1.0D0/DINV(I)
 50   CONTINUE
C
      RETURN
C------------- LAST LINE OF DSD2S FOLLOWS ----------------------------
      END
