      SUBROUTINE SCHKW (NAME, LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR)
C     .. Scalar Arguments ..
      REAL ERR
      INTEGER IERR, ITER, LENIW, LENW, LOCIW, LOCW
      CHARACTER NAME*(*)
C     .. Local Scalars ..
      CHARACTER XERN1*8, XERN2*8, XERNAM*8
C     .. External Functions ..
      REAL R1MACH
      EXTERNAL R1MACH
C     .. External Subroutines ..
      EXTERNAL XERMSG
C***FIRST EXECUTABLE STATEMENT  SCHKW
C
C         Check the Integer workspace situation.
C
      IERR = 0
      ITER = 0
      ERR = R1MACH(1)
      IF( LOCIW.GT.LENIW ) THEN
         IERR = 1
         ERR = R1MACH(2)
         XERNAM = NAME
         WRITE (XERN1, '(I8)') LOCIW
         WRITE (XERN2, '(I8)') LENIW
         CALL XERMSG ('SLATEC', 'SCHKW',
     $      'In ' // XERNAM // ', INTEGER work array too short.  ' //
     $      'IWORK needs ' // XERN1 // '; have allocated ' // XERN2,
     $      1, 1)
      ENDIF
C
C         Check the Real workspace situation.
      IF( LOCW.GT.LENW ) THEN
         IERR = 1
         ERR = R1MACH(2)
         XERNAM = NAME
         WRITE (XERN1, '(I8)') LOCW
         WRITE (XERN2, '(I8)') LENW
         CALL XERMSG ('SLATEC', 'SCHKW',
     $      'In ' // XERNAM // ', REAL work array too short.  ' //
     $      'RWORK needs ' // XERN1 // '; have allocated ' // XERN2,
     $      1, 1)
      ENDIF
      RETURN
C------------- LAST LINE OF SCHKW FOLLOWS ----------------------------
      END
