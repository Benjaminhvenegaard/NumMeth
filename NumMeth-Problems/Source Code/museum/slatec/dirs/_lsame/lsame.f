      LOGICAL FUNCTION LSAME (CA, CB)
C     .. Scalar Arguments ..
      CHARACTER CA*1, CB*1
C     .. Local Scalars ..
      INTEGER IOFF
      LOGICAL FIRST
C     .. Intrinsic Functions ..
      INTRINSIC ICHAR
C     .. Save statement ..
      SAVE FIRST, IOFF
C     .. Data statements ..
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  LSAME
      IF (FIRST) IOFF = ICHAR('a') - ICHAR('A')
C
      FIRST = .FALSE.
C
C     Test if the characters are equal or equivalent.
C
      LSAME = (CA.EQ.CB) .OR. (ICHAR(CA)-IOFF.EQ.ICHAR(CB))
C
      RETURN
C
C  The following comments contain code for CDC systems using 6-12 bit
C  representations.
C
C     .. Parameters ..
C     INTEGER                ICIRFX
C     PARAMETER            ( ICIRFX=62 )
C     .. Scalar Arguments ..
C     CHARACTER*1            CB
C     .. Array Arguments ..
C     CHARACTER*1            CA(*)
C     .. Local Scalars ..
C     INTEGER                IVAL
C     .. Intrinsic Functions ..
C     INTRINSIC              ICHAR, CHAR
C     .. Executable Statements ..
C     INTRINSIC              ICHAR, CHAR
C
C     See if the first character in string CA equals string CB.
C
C     LSAME = CA(1) .EQ. CB .AND. CA(1) .NE. CHAR(ICIRFX)
C
C     IF (LSAME) RETURN
C
C     The characters are not identical. Now check them for equivalence.
C     Look for the 'escape' character, circumflex, followed by the
C     letter.
C
C     IVAL = ICHAR(CA(2))
C     IF (IVAL.GE.ICHAR('A') .AND. IVAL.LE.ICHAR('Z')) THEN
C        LSAME = CA(1) .EQ. CHAR(ICIRFX) .AND. CA(2) .EQ. CB
C     ENDIF
C
C     RETURN
C
C     End of LSAME.
C
      END
