      SUBROUTINE DCHKW (NAME, LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR
      INTEGER IERR, ITER, LENIW, LENW, LOCIW, LOCW
      CHARACTER NAME*(*)
C     .. Local Scalars ..
      CHARACTER XERN1*8, XERN2*8, XERNAM*8
C     .. External Functions ..
      DOUBLE PRECISION D1MACH
      EXTERNAL D1MACH
C     .. External Subroutines ..
      EXTERNAL XERMSG
C***FIRST EXECUTABLE STATEMENT  DCHKW
C
C         Check the Integer workspace situation.
C
      IERR = 0
      ITER = 0
      ERR = D1MACH(1)
      IF( LOCIW.GT.LENIW ) THEN
         IERR = 1
         ERR = D1MACH(2)
         XERNAM = NAME
         WRITE (XERN1, '(I8)') LOCIW
         WRITE (XERN2, '(I8)') LENIW
         CALL XERMSG ('SLATEC', 'DCHKW',
     $      'In ' // XERNAM // ', INTEGER work array too short.  ' //
     $      'IWORK needs ' // XERN1 // '; have allocated ' // XERN2,
     $      1, 1)
      ENDIF
C
C         Check the Double Precision workspace situation.
      IF( LOCW.GT.LENW ) THEN
         IERR = 1
         ERR = D1MACH(2)
         XERNAM = NAME
         WRITE (XERN1, '(I8)') LOCW
         WRITE (XERN2, '(I8)') LENW
         CALL XERMSG ('SLATEC', 'DCHKW',
     $      'In ' // XERNAM // ', DOUBLE PRECISION work array too ' //
     $      'short.  RWORK needs ' // XERN1 // '; have allocated ' //
     $      XERN2, 1, 1)
      ENDIF
      RETURN
C------------- LAST LINE OF DCHKW FOLLOWS ----------------------------
      END
