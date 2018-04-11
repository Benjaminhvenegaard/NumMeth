      SUBROUTINE DDRIV2 (N, T, Y, F, TOUT, MSTATE, NROOT, EPS, EWT,
     8   MINT, WORK, LENW, IWORK, LENIW, G, IERFLG)
      EXTERNAL F, G
      DOUBLE PRECISION EPS, EWT, EWTCOM(1), G, HMAX, T, TOUT,
     8     WORK(*), Y(*)
      INTEGER IWORK(*)
      INTEGER IERFLG, IERROR, IMPL, LENIW, LENW, MINT, MITER, ML,
     8        MSTATE, MU, MXORD, MXSTEP, N, NDE, NROOT, NSTATE, NTASK
      CHARACTER INTGR1*8
      PARAMETER(IMPL = 0, MXSTEP = 1000)
C***FIRST EXECUTABLE STATEMENT  DDRIV2
      IF (ABS(MSTATE) .EQ. 9) THEN
        IERFLG = 999
        CALL XERMSG('SLATEC', 'DDRIV2',
     8  'Illegal input.  The magnitude of MSTATE IS 9 .',
     8  IERFLG, 2)
        RETURN
      ELSE IF (ABS(MSTATE) .EQ. 0 .OR. ABS(MSTATE) .GT. 9) THEN
        WRITE(INTGR1, '(I8)') MSTATE
        IERFLG = 26
        CALL XERMSG('SLATEC', 'DDRIV2',
     8  'Illegal input.  The magnitude of MSTATE, '//INTGR1//
     8  ' is not in the range 1 to 8 .', IERFLG, 1)
        MSTATE = SIGN(9, MSTATE)
        RETURN
      END IF
      IF (MINT .LT. 1 .OR. MINT .GT. 3) THEN
        WRITE(INTGR1, '(I8)') MINT
        IERFLG = 23
        CALL XERMSG('SLATEC', 'DDRIV2',
     8  'Illegal input.  Improper value for the integration method '//
     8  'flag, '//INTGR1//' .', IERFLG, 1)
        MSTATE = SIGN(9, MSTATE)
        RETURN
      END IF
      IF (MSTATE .GE. 0) THEN
        NSTATE = MSTATE
        NTASK = 1
      ELSE
        NSTATE = - MSTATE
        NTASK = 3
      END IF
      EWTCOM(1) = EWT
      IF (EWT .NE. 0.D0) THEN
        IERROR = 3
      ELSE
        IERROR = 2
      END IF
      IF (MINT .EQ. 1) THEN
        MITER = 0
        MXORD = 12
      ELSE IF (MINT .EQ. 2) THEN
        MITER = 2
        MXORD = 5
      ELSE IF (MINT .EQ. 3) THEN
        MITER = 2
        MXORD = 12
      END IF
      HMAX = 2.D0*ABS(TOUT - T)
      CALL DDRIV3 (N, T, Y, F, NSTATE, TOUT, NTASK, NROOT, EPS, EWTCOM,
     8             IERROR, MINT, MITER, IMPL, ML, MU, MXORD, HMAX, WORK,
     8             LENW, IWORK, LENIW, F, F, NDE, MXSTEP, G, F, IERFLG)
      IF (NSTATE .LE. 7) THEN
        MSTATE = SIGN(NSTATE, MSTATE)
      ELSE IF (NSTATE .EQ. 11) THEN
        MSTATE = SIGN(8, MSTATE)
      ELSE IF (NSTATE .GT. 11) THEN
        MSTATE = SIGN(9, MSTATE)
      END IF
      RETURN
      END
