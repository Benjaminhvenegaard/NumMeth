      SUBROUTINE DS2Y (N, NELT, IA, JA, A, ISYM)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER I, IBGN, ICOL, IEND, ITEMP, J
C     .. External Subroutines ..
      EXTERNAL QS2I1D
C***FIRST EXECUTABLE STATEMENT  DS2Y
C
C         Check to see if the (IA,JA,A) arrays are in SLAP Column
C         format.  If it's not then transform from SLAP Triad.
C
      IF( JA(N+1).EQ.NELT+1 ) RETURN
C
C         Sort into ascending order by COLUMN (on the ja array).
C         This will line up the columns.
C
      CALL QS2I1D( JA, IA, A, NELT, 1 )
C
C         Loop over each column to see where the column indices change
C         in the column index array ja.  This marks the beginning of the
C         next column.
C
CVD$R NOVECTOR
      JA(1) = 1
      DO 20 ICOL = 1, N-1
         DO 10 J = JA(ICOL)+1, NELT
            IF( JA(J).NE.ICOL ) THEN
               JA(ICOL+1) = J
               GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
      JA(N+1) = NELT+1
C
C         Mark the n+2 element so that future calls to a SLAP routine
C         utilizing the YSMP-Column storage format will be able to tell.
C
      JA(N+2) = 0
C
C         Now loop through the IA array making sure that the diagonal
C         matrix element appears first in the column.  Then sort the
C         rest of the column in ascending order.
C
      DO 70 ICOL = 1, N
         IBGN = JA(ICOL)
         IEND = JA(ICOL+1)-1
         DO 30 I = IBGN, IEND
            IF( IA(I).EQ.ICOL ) THEN
C
C              Swap the diagonal element with the first element in the
C              column.
C
               ITEMP = IA(I)
               IA(I) = IA(IBGN)
               IA(IBGN) = ITEMP
               TEMP = A(I)
               A(I) = A(IBGN)
               A(IBGN) = TEMP
               GOTO 40
            ENDIF
 30      CONTINUE
 40      IBGN = IBGN + 1
         IF( IBGN.LT.IEND ) THEN
            DO 60 I = IBGN, IEND
               DO 50 J = I+1, IEND
                  IF( IA(I).GT.IA(J) ) THEN
                     ITEMP = IA(I)
                     IA(I) = IA(J)
                     IA(J) = ITEMP
                     TEMP = A(I)
                     A(I) = A(J)
                     A(J) = TEMP
                  ENDIF
 50            CONTINUE
 60         CONTINUE
         ENDIF
 70   CONTINUE
      RETURN
C------------- LAST LINE OF DS2Y FOLLOWS ----------------------------
      END
