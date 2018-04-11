      SUBROUTINE SDRIV1 (N, T, Y, F, TOUT, MSTATE, EPS, WORK, LENW,
     8   IERFLG)
      EXTERNAL F
      REAL EPS, EWTCOM(1), HMAX, T, TOUT, WORK(*), Y(*)
      INTEGER I, IDLIW, IERFLG, IERROR, IMPL, LENIW, LENW, LENWCM,
     8        LNWCHK, MINT, MITER, ML, MSTATE, MU, MXN, MXORD, MXSTEP,
     8        N, NDE, NROOT, NSTATE, NTASK
      PARAMETER(MXN = 200, IDLIW = 50)
      INTEGER IWORK(IDLIW+MXN)
      CHARACTER INTGR1*8
      PARAMETER(NROOT = 0, IERROR = 2, MINT = 2, MITER = 2, IMPL = 0,
     8          MXORD = 5, MXSTEP = 1000)
      DATA EWTCOM(1) /1.E0/
C***FIRST EXECUTABLE STATEMENT  SDRIV1
      IF (ABS(MSTATE) .EQ. 0 .OR. ABS(MSTATE) .GT. 7) THEN
        WRITE(INTGR1, '(I8)') MSTATE
        IERFLG = 26
        CALL XERMSG('SLATEC', 'SDRIV1',
     8  'Illegal input.  The magnitude of MSTATE, '//INTGR1//
     8  ', is not in the range 1 to 6 .', IERFLG, 1)
        MSTATE = SIGN(7, MSTATE)
        RETURN
      ELSE IF (ABS(MSTATE) .EQ. 7) THEN
        IERFLG = 999
        CALL XERMSG('SLATEC', 'SDRIV1',
     8  'Illegal input.  The magnitude of MSTATE is 7 .', IERFLG, 2)
        RETURN
      END IF
      IF (N .GT. MXN) THEN
        WRITE(INTGR1, '(I8)') N
        IERFLG = 21
        CALL XERMSG('SLATEC', 'SDRIV1',
     8  'Illegal input.  The number of equations, '//INTGR1//
     8  ', is greater than the maximum allowed: 200 .', IERFLG, 1)
        MSTATE = SIGN(7, MSTATE)
        RETURN
      END IF
      IF (MSTATE .GT. 0) THEN
        NSTATE = MSTATE
        NTASK = 1
      ELSE
        NSTATE = - MSTATE
        NTASK = 3
      END IF
      HMAX = 2.E0*ABS(TOUT - T)
      LENIW = N + IDLIW
      LENWCM = LENW - LENIW
      IF (LENWCM .LT. (N*N + 10*N + 250)) THEN
        LNWCHK = N*N + 10*N + 250 + LENIW
        WRITE(INTGR1, '(I8)') LNWCHK
        IERFLG = 32
        CALL XERMSG('SLATEC', 'SDRIV1',
     8  'Insufficient storage allocated for the work array.  '//
     8  'The required storage is at least '//INTGR1//' .', IERFLG, 1)
        MSTATE = SIGN(7, MSTATE)
        RETURN
      END IF
      IF (NSTATE .NE. 1) THEN
        DO 20 I = 1,LENIW
 20       IWORK(I) = WORK(I+LENWCM)
      END IF
      CALL SDRIV3 (N, T, Y, F, NSTATE, TOUT, NTASK, NROOT, EPS, EWTCOM,
     8             IERROR, MINT, MITER, IMPL, ML, MU, MXORD, HMAX, WORK,
     8             LENWCM, IWORK, LENIW, F, F, NDE, MXSTEP, F, F,
     8             IERFLG)
      DO 40 I = 1,LENIW
 40     WORK(I+LENWCM) = IWORK(I)
      IF (NSTATE .LE. 4) THEN
        MSTATE = SIGN(NSTATE, MSTATE)
      ELSE IF (NSTATE .EQ. 6) THEN
        MSTATE = SIGN(5, MSTATE)
      ELSE IF (IERFLG .EQ. 11) THEN
        MSTATE = SIGN(6, MSTATE)
      ELSE IF (IERFLG .GT. 11) THEN
        MSTATE = SIGN(7, MSTATE)
      END IF
      RETURN
      END
