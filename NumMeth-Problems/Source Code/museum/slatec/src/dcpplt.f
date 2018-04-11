      SUBROUTINE DCPPLT (N, NELT, IA, JA, A, ISYM, IUNIT)
C     .. Scalar Arguments ..
      INTEGER ISYM, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT)
      INTEGER IA(NELT), JA(NELT)
C     .. Parameters ..
      INTEGER  MAXORD
      PARAMETER (MAXORD=225)
C     .. Local Scalars ..
      INTEGER I, ICOL, IROW, J, JBGN, JEND, NMAX
C     .. Local Arrays ..
      CHARACTER CHMAT(MAXORD)*(MAXORD)
C     .. Intrinsic Functions ..
      INTRINSIC MIN, MOD, REAL
C***FIRST EXECUTABLE STATEMENT  DCPPLT
C
C         Set up the character matrix...
C
      NMAX = MIN( MAXORD, N )
      DO 10 I = 1, NMAX
         CHMAT(I)(1:NMAX) = ' '
 10   CONTINUE
      DO 30 ICOL = 1, NMAX
         JBGN = JA(ICOL)
         JEND = JA(ICOL+1)-1
         DO 20 J = JBGN, JEND
            IROW = IA(J)
            IF( IROW.LE.NMAX ) THEN
               IF( ISYM.NE.0 ) THEN
C         Put in non-sym part as well...
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '#'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = '*'
                  ENDIF
               ENDIF
               IF( IROW.EQ.ICOL ) THEN
C         Diagonal entry.
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = 'D'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = 'N'
                  ENDIF
               ELSE
C         Off-Diagonal entry
                  IF( A(J).EQ.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '0'
                  ELSEIF( A(J).GT.0.0D0 ) THEN
                     CHMAT(IROW)(ICOL:ICOL) = '#'
                  ELSE
                     CHMAT(IROW)(ICOL:ICOL) = '*'
                  ENDIF
               ENDIF
            ENDIF
 20      CONTINUE
 30   CONTINUE
C
C         Write out the heading.
      WRITE(IUNIT,1000) N, NELT, REAL(NELT)/(N*N)
      WRITE(IUNIT,1010) (MOD(I,10),I=1,NMAX)
C
C         Write out the character representations matrix elements.
      DO 40 IROW = 1, NMAX
         WRITE(IUNIT,1020) IROW, CHMAT(IROW)(1:NMAX)
 40   CONTINUE
      RETURN
C
 1000 FORMAT(/'**** Picture of Column SLAP matrix follows ****'/
     $     ' N, NELT and Density = ',2I10,D16.7)
C      The following assumes MAXORD.le.225.
 1010 FORMAT(4X,225(I1))
 1020 FORMAT(1X,I3,A)
C------------- LAST LINE OF DCPPLT FOLLOWS ----------------------------
      END
