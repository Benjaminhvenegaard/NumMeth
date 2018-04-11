      SUBROUTINE DDRIV3 (N, T, Y, F, NSTATE, TOUT, NTASK, NROOT, EPS,
     8   EWT, IERROR, MINT, MITER, IMPL, ML, MU, MXORD, HMAX, WORK,
     8   LENW, IWORK, LENIW, JACOBN, FA, NDE, MXSTEP, G, USERS, IERFLG)
      EXTERNAL F, JACOBN, FA, G, USERS
      DOUBLE PRECISION AE, BIG, EPS, EWT(*), G, GLAST, GNOW, H, HMAX,
     8     HSIGN, HUSED, NROUND, RE, D1MACH, SIZE, DNRM2, SUM, T, TLAST,
     8     TOUT, TROOT, UROUND, WORK(*), Y(*)
      INTEGER I, IA, IAVGH, IAVGRD, ICNVRG, IDFDY, IEL, IERFLG, IERROR,
     8        IFAC, IFLAG, IGNOW, IH, IHMAX, IHOLD, IHSIGN, IHUSED,
     8        IJROOT, IJSTPL, IJTASK, IMNT, IMNTLD, IMPL, IMTR, IMTRLD,
     8        IMTRSV, IMXERR, IMXORD, IMXRDS, INDMXR, INDPRT, INDPVT,
     8        INDTRT, INFE, INFO, INJE, INQ, INQUSE, INROOT, INRTLD,
     8        INSTEP, INWAIT, IRC, IRMAX, IROOT, IMACH1, IMACH4, ISAVE1,
     8        ISAVE2, IT, ITOUT, ITQ, ITREND, ITROOT, IWORK(*), IYH,
     8        IYWT, J, JSTATE, JTROOT, LENCHK, LENIW, LENW, LIWCHK,
     8        MATDIM, MAXORD, MINT, MITER, ML, MU, MXORD, MXSTEP, N,
     8        NDE, NDECOM, NPAR, NROOT, NSTATE, NSTEPL, NTASK
      LOGICAL CONVRG
      CHARACTER INTGR1*8, INTGR2*8, RL1*16, RL2*16
      PARAMETER(NROUND = 20.D0)
      PARAMETER(IAVGH = 1, IHUSED = 2, IAVGRD = 3,
     8          IEL = 4, IH = 160, IHMAX = 161, IHOLD = 162,
     8          IHSIGN = 163, IRC = 164, IRMAX = 165, IT = 166,
     8          ITOUT = 167, ITQ = 168, ITREND = 204, IMACH1 = 205,
     8          IMACH4 = 206, IYH = 251,
     8          INDMXR = 1, INQUSE = 2, INSTEP = 3, INFE = 4, INJE = 5,
     8          INROOT = 6, ICNVRG = 7, IJROOT = 8, IJTASK = 9,
     8          IMNTLD = 10, IMTRLD = 11, INQ = 12, INRTLD = 13,
     8          INDTRT = 14, INWAIT = 15, IMNT = 16, IMTRSV = 17,
     8          IMTR = 18, IMXRDS = 19, IMXORD = 20, INDPRT = 21,
     8          IJSTPL = 22, INDPVT = 51)
C***FIRST EXECUTABLE STATEMENT  DDRIV3
      IF (NSTATE .EQ. 12) THEN
        IERFLG = 999
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  The value of NSTATE is 12 .', IERFLG, 2)
        RETURN
      ELSE IF (NSTATE .LT. 1 .OR. NSTATE .GT. 12) THEN
        WRITE(INTGR1, '(I8)') NSTATE
        IERFLG = 26
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Improper value for NSTATE(= '//INTGR1//').',
     8  IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      NPAR = N
      IF (EPS .LT. 0.D0) THEN
        WRITE(RL1, '(D16.8)') EPS
        IERFLG = 27
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  EPS, '//RL1//', is negative.', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      IF (N .LE. 0) THEN
        WRITE(INTGR1, '(I8)') N
        IERFLG = 22
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Number of equations, '//INTGR1//
     8  ', is not positive.', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      IF (MXORD .LE. 0) THEN
        WRITE(INTGR1, '(I8)') MXORD
        IERFLG = 28
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Maximum order, '//INTGR1//
     8  ', is not positive.', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      IF (MINT .LT. 1 .OR. MINT .GT. 3) THEN
        WRITE(INTGR1, '(I8)') MINT
        IERFLG = 23
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Improper value for the integration method '//
     8  'flag, '//INTGR1//' .', IERFLG, 1)
        NSTATE = 12
        RETURN
      ELSE IF (MITER .LT. 0 .OR. MITER .GT. 5) THEN
        WRITE(INTGR1, '(I8)') MITER
        IERFLG = 24
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Improper value for MITER(= '//INTGR1//').',
     8  IERFLG, 1)
        NSTATE = 12
        RETURN
      ELSE IF (IMPL .LT. 0 .OR. IMPL .GT. 3) THEN
        WRITE(INTGR1, '(I8)') IMPL
        IERFLG = 25
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Improper value for IMPL(= '//INTGR1//').',
     8  IERFLG, 1)
        NSTATE = 12
        RETURN
      ELSE IF (MINT .EQ. 3 .AND.
     8  (MITER .EQ. 0 .OR. MITER .EQ. 3 .OR. IMPL .NE. 0)) THEN
        WRITE(INTGR1, '(I8)') MITER
        WRITE(INTGR2, '(I8)') IMPL
        IERFLG = 29
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  For MINT = 3, the value of MITER, '//INTGR1//
     8  ', and/or IMPL, '//INTGR2//', is not allowed.', IERFLG, 1)
        NSTATE = 12
        RETURN
      ELSE IF ((IMPL .GE. 1 .AND. IMPL .LE. 3) .AND. MITER .EQ. 0) THEN
        WRITE(INTGR1, '(I8)') IMPL
        IERFLG = 30
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  For MITER = 0, the value of IMPL, '//INTGR1//
     8  ', is not allowed.', IERFLG, 1)
        NSTATE = 12
        RETURN
      ELSE IF ((IMPL .EQ. 2 .OR. IMPL .EQ. 3) .AND. MINT .EQ. 1) THEN
        WRITE(INTGR1, '(I8)') IMPL
        IERFLG = 31
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  For MINT = 1, the value of IMPL, '//INTGR1//
     8  ', is not allowed.', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) THEN
        LIWCHK = INDPVT - 1
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2 .OR. MITER .EQ. 4 .OR.
     8  MITER .EQ. 5) THEN
        LIWCHK = INDPVT + N - 1
      END IF
      IF (LENIW .LT. LIWCHK) THEN
        WRITE(INTGR1, '(I8)') LIWCHK
        IERFLG = 33
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Insufficient storage allocated for the '//
     8  'IWORK array.  Based on the value of the input parameters '//
     8  'involved, the required storage is '//INTGR1//' .', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
C                                                Allocate the WORK array
C                                         IYH is the index of YH in WORK
      IF (MINT .EQ. 1 .OR. MINT .EQ. 3) THEN
        MAXORD = MIN(MXORD, 12)
      ELSE IF (MINT .EQ. 2) THEN
        MAXORD = MIN(MXORD, 5)
      END IF
      IDFDY = IYH + (MAXORD + 1)*N
C                                             IDFDY is the index of DFDY
C
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) THEN
        IYWT = IDFDY
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        IYWT = IDFDY + N*N
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        IYWT = IDFDY + (2*ML + MU + 1)*N
      END IF
C                                               IYWT is the index of YWT
      ISAVE1 = IYWT + N
C                                           ISAVE1 is the index of SAVE1
      ISAVE2 = ISAVE1 + N
C                                           ISAVE2 is the index of SAVE2
      IGNOW = ISAVE2 + N
C                                             IGNOW is the index of GNOW
      ITROOT = IGNOW + NROOT
C                                           ITROOT is the index of TROOT
      IFAC = ITROOT + NROOT
C                                               IFAC is the index of FAC
      IF (MITER .EQ. 2 .OR. MITER .EQ. 5 .OR. MINT .EQ. 3) THEN
        IA = IFAC + N
      ELSE
        IA = IFAC
      END IF
C                                                   IA is the index of A
      IF (IMPL .EQ. 0 .OR. MITER .EQ. 3) THEN
        LENCHK = IA - 1
      ELSE IF (IMPL .EQ. 1 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
        LENCHK = IA - 1 + N*N
      ELSE IF (IMPL .EQ. 1 .AND. (MITER .EQ. 4 .OR. MITER .EQ. 5)) THEN
        LENCHK = IA - 1 + (2*ML + MU + 1)*N
      ELSE IF (IMPL .EQ. 2 .AND. MITER .NE. 3) THEN
        LENCHK = IA - 1 + N
      ELSE IF (IMPL .EQ. 3 .AND. (MITER .EQ. 1 .OR. MITER .EQ. 2)) THEN
        LENCHK = IA - 1 + N*NDE
      ELSE IF (IMPL .EQ. 3 .AND. (MITER .EQ. 4 .OR. MITER .EQ. 5)) THEN
        LENCHK = IA - 1 + (2*ML + MU + 1)*NDE
      END IF
      IF (LENW .LT. LENCHK) THEN
        WRITE(INTGR1, '(I8)') LENCHK
        IERFLG = 32
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'Illegal input.  Insufficient storage allocated for the '//
     8  'WORK array.  Based on the value of the input parameters '//
     8  'involved, the required storage is '//INTGR1//' .', IERFLG, 1)
        NSTATE = 12
        RETURN
      END IF
      IF (MITER .EQ. 0 .OR. MITER .EQ. 3) THEN
        MATDIM = 1
      ELSE IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
        MATDIM = N
      ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
        MATDIM = 2*ML + MU + 1
      END IF
      IF (IMPL .EQ. 0 .OR. IMPL .EQ. 1) THEN
        NDECOM = N
      ELSE IF (IMPL .EQ. 2 .OR. IMPL .EQ. 3) THEN
        NDECOM = NDE
      END IF
      IF (NSTATE .EQ. 1) THEN
C                                                  Initialize parameters
        IF (MINT .EQ. 1 .OR. MINT .EQ. 3) THEN
          IWORK(IMXORD) = MIN(MXORD, 12)
        ELSE IF (MINT .EQ. 2) THEN
          IWORK(IMXORD) = MIN(MXORD, 5)
        END IF
        IWORK(IMXRDS) = MXORD
        IF (MINT .EQ. 1 .OR. MINT .EQ. 2) THEN
          IWORK(IMNT) = MINT
          IWORK(IMTR) = MITER
          IWORK(IMNTLD) = MINT
          IWORK(IMTRLD) = MITER
        ELSE IF (MINT .EQ. 3) THEN
          IWORK(IMNT) = 1
          IWORK(IMTR) = 0
          IWORK(IMNTLD) = IWORK(IMNT)
          IWORK(IMTRLD) = IWORK(IMTR)
          IWORK(IMTRSV) = MITER
        END IF
        WORK(IHMAX) = HMAX
        UROUND = D1MACH (4)
        WORK(IMACH4) = UROUND
        WORK(IMACH1) = D1MACH (1)
        IF (NROOT .NE. 0) THEN
          RE = UROUND
          AE = WORK(IMACH1)
        END IF
        H = (TOUT - T)*(1.D0 - 4.D0*UROUND)
        H = SIGN(MIN(ABS(H), HMAX), H)
        WORK(IH) = H
        HSIGN = SIGN(1.D0, H)
        WORK(IHSIGN) = HSIGN
        IWORK(IJTASK) = 0
        WORK(IAVGH) = 0.D0
        WORK(IHUSED) = 0.D0
        WORK(IAVGRD) = 0.D0
        IWORK(INDMXR) = 0
        IWORK(INQUSE) = 0
        IWORK(INSTEP) = 0
        IWORK(IJSTPL) = 0
        IWORK(INFE) = 0
        IWORK(INJE) = 0
        IWORK(INROOT) = 0
        WORK(IT) = T
        IWORK(ICNVRG) = 0
        IWORK(INDPRT) = 0
C                                                 Set initial conditions
        DO 30 I = 1,N
 30       WORK(I+IYH-1) = Y(I)
        IF (T .EQ. TOUT) RETURN
        GO TO 180
      ELSE
        UROUND = WORK(IMACH4)
        IF (NROOT .NE. 0) THEN
          RE = UROUND
          AE = WORK(IMACH1)
        END IF
      END IF
C                                             On a continuation, check
C                                             that output points have
C                                             been or will be overtaken.
      IF (IWORK(ICNVRG) .EQ. 1) THEN
        CONVRG = .TRUE.
      ELSE
        CONVRG = .FALSE.
      END IF
      T = WORK(IT)
      H = WORK(IH)
      HSIGN = WORK(IHSIGN)
      IF (IWORK(IJTASK) .EQ. 0) GO TO 180
C
C                                   IWORK(IJROOT) flags unreported
C                                   roots, and is set to the value of
C                                   NTASK when a root was last selected.
C                                   It is set to zero when all roots
C                                   have been reported.  IWORK(INROOT)
C                                   contains the index and WORK(ITOUT)
C                                   contains the value of the root last
C                                   selected to be reported.
C                                   IWORK(INRTLD) contains the value of
C                                   NROOT and IWORK(INDTRT) contains
C                                   the value of ITROOT when the array
C                                   of roots was last calculated.
      IF (NROOT .NE. 0) THEN
        IF (IWORK(IJROOT) .GT. 0) THEN
C                                      TOUT has just been reported.
C                                      If TROOT .LE. TOUT, report TROOT.
          IF (NSTATE .NE. 5) THEN
            IF (TOUT*HSIGN .GE. WORK(ITOUT)*HSIGN) THEN
              TROOT = WORK(ITOUT)
              CALL DDNTP (H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),  Y)
              T = TROOT
              NSTATE = 5
              IERFLG = 0
              GO TO 580
            END IF
C                                         A root has just been reported.
C                                         Select the next root.
          ELSE
            TROOT = T
            IROOT = 0
            DO 50 I = 1,IWORK(INRTLD)
              JTROOT = I + IWORK(INDTRT) - 1
              IF (WORK(JTROOT)*HSIGN .LE. TROOT*HSIGN) THEN
C
C                                              Check for multiple roots.
C
                IF (WORK(JTROOT) .EQ. WORK(ITOUT) .AND.
     8          I .GT. IWORK(INROOT)) THEN
                  IROOT = I
                  TROOT = WORK(JTROOT)
                  GO TO 60
                END IF
                IF (WORK(JTROOT)*HSIGN .GT. WORK(ITOUT)*HSIGN) THEN
                  IROOT = I
                  TROOT = WORK(JTROOT)
                END IF
              END IF
 50           CONTINUE
 60         IWORK(INROOT) = IROOT
            WORK(ITOUT) = TROOT
            IWORK(IJROOT) = NTASK
            IF (NTASK .EQ. 1) THEN
              IF (IROOT .EQ. 0) THEN
                IWORK(IJROOT) = 0
              ELSE
                IF (TOUT*HSIGN .GE. TROOT*HSIGN) THEN
                  CALL DDNTP (H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),
     8                        Y)
                  NSTATE = 5
                  T = TROOT
                  IERFLG = 0
                  GO TO 580
                END IF
              END IF
            ELSE IF (NTASK .EQ. 2 .OR. NTASK .EQ. 3) THEN
C
C                                     If there are no more roots, or the
C                                     user has altered TOUT to be less
C                                     than a root, set IJROOT to zero.
C
              IF (IROOT .EQ. 0 .OR. (TOUT*HSIGN .LT. TROOT*HSIGN)) THEN
                IWORK(IJROOT) = 0
              ELSE
                CALL DDNTP (H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),
     8                      Y)
                NSTATE = 5
                IERFLG = 0
                T = TROOT
                GO TO 580
              END IF
            END IF
          END IF
        END IF
      END IF
C
      IF (NTASK .EQ. 1) THEN
        NSTATE = 2
        IF (T*HSIGN .GE. TOUT*HSIGN) THEN
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          IERFLG = 0
          GO TO 580
        END IF
      ELSE IF (NTASK .EQ. 2) THEN
C                                                      Check if TOUT has
C                                                      been reset .LT. T
        IF (T*HSIGN .GT. TOUT*HSIGN) THEN
          WRITE(RL1, '(D16.8)') T
          WRITE(RL2, '(D16.8)') TOUT
          IERFLG = 11
          CALL XERMSG('SLATEC', 'DDRIV3',
     8    'While integrating exactly to TOUT, T, '//RL1//
     8    ', was beyond TOUT, '//RL2//' .  Solution obtained by '//
     8    'interpolation.', IERFLG, 0)
          NSTATE = 11
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          GO TO 580
        END IF
C                                   Determine if TOUT has been overtaken
C
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
          NSTATE = 2
          IERFLG = 0
          GO TO 560
        END IF
C                                             If there are no more roots
C                                             to report, report T.
        IF (NSTATE .EQ. 5) THEN
          NSTATE = 2
          IERFLG = 0
          GO TO 560
        END IF
        NSTATE = 2
C                                                       See if TOUT will
C                                                       be overtaken.
        IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
          H = TOUT - T
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
          WORK(IH) = H
          IF (H .EQ. 0.D0) GO TO 670
          IWORK(IJTASK) = -1
        END IF
      ELSE IF (NTASK .EQ. 3) THEN
        NSTATE = 2
        IF (T*HSIGN .GT. TOUT*HSIGN) THEN
          WRITE(RL1, '(D16.8)') T
          WRITE(RL2, '(D16.8)') TOUT
          IERFLG = 11
          CALL XERMSG('SLATEC', 'DDRIV3',
     8    'While integrating exactly to TOUT, T, '//RL1//
     8    ', was beyond TOUT, '//RL2//' .  Solution obtained by '//
     8    'interpolation.', IERFLG, 0)
          NSTATE = 11
          CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
          T = TOUT
          GO TO 580
        END IF
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
          IERFLG = 0
          GO TO 560
        END IF
        IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
          H = TOUT - T
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
          WORK(IH) = H
          IF (H .EQ. 0.D0) GO TO 670
          IWORK(IJTASK) = -1
        END IF
      END IF
C                         Implement changes in MINT, MITER, and/or HMAX.
C
      IF ((MINT .NE. IWORK(IMNTLD) .OR. MITER .NE. IWORK(IMTRLD)) .AND.
     8  MINT .NE. 3 .AND. IWORK(IMNTLD) .NE. 3) IWORK(IJTASK) = -1
      IF (HMAX .NE. WORK(IHMAX)) THEN
        H = SIGN(MIN(ABS(H), HMAX), H)
        IF (H .NE. WORK(IH)) THEN
          IWORK(IJTASK) = -1
          WORK(IH) = H
        END IF
        WORK(IHMAX) = HMAX
      END IF
C
 180  NSTEPL = IWORK(INSTEP)
      DO 190 I = 1,N
 190    Y(I) = WORK(I+IYH-1)
      IF (NROOT .NE. 0) THEN
        DO 200 I = 1,NROOT
          WORK(I+IGNOW-1) = G (NPAR, T, Y, I)
          IF (NPAR .EQ. 0) THEN
            IWORK(INROOT) = I
            NSTATE = 7
            RETURN
          END IF
 200     CONTINUE
      END IF
      IF (IERROR .EQ. 1) THEN
        DO 230 I = 1,N
 230      WORK(I+IYWT-1) = 1.D0
        GO TO 410
      ELSE IF (IERROR .EQ. 5) THEN
        DO 250 I = 1,N
 250      WORK(I+IYWT-1) = EWT(I)
        GO TO 410
      END IF
C                                       Reset YWT array.  Looping point.
 260  IF (IERROR .EQ. 2) THEN
        DO 280 I = 1,N
          IF (Y(I) .EQ. 0.D0) GO TO 290
 280      WORK(I+IYWT-1) = ABS(Y(I))
        GO TO 410
 290    IF (IWORK(IJTASK) .EQ. 0) THEN
          CALL F (NPAR, T, Y, WORK(ISAVE2))
          IF (NPAR .EQ. 0) THEN
            NSTATE = 6
            RETURN
          END IF
          IWORK(INFE) = IWORK(INFE) + 1
          IF (MITER .EQ. 3 .AND. IMPL .NE. 0) THEN
            IFLAG = 0
            CALL USERS (Y, WORK(IYH), WORK(IYWT), WORK(ISAVE1),
     8                  WORK(ISAVE2), T, H, WORK(IEL), IMPL, NPAR,
     8                  NDECOM, IFLAG)
            IF (IFLAG .EQ. -1) GO TO 690
            IF (NPAR .EQ. 0) THEN
              NSTATE = 10
              RETURN
            END IF
          ELSE IF (IMPL .EQ. 1) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (NPAR, T, Y, WORK(IA), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGEFA (WORK(IA), MATDIM, N, IWORK(INDPVT), INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGESL (WORK(IA), MATDIM, N, IWORK(INDPVT),
     8                    WORK(ISAVE2), 0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              CALL FA (NPAR, T, Y, WORK(IA+ML), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGBFA (WORK(IA), MATDIM, N, ML, MU, IWORK(INDPVT),
     8                    INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGBSL (WORK(IA), MATDIM, N, ML, MU, IWORK(INDPVT),
     8                    WORK(ISAVE2), 0)
            END IF
          ELSE IF (IMPL .EQ. 2) THEN
            CALL FA (NPAR, T, Y, WORK(IA), MATDIM, ML, MU, NDECOM)
            IF (NPAR .EQ. 0) THEN
              NSTATE = 9
              RETURN
            END IF
            DO 340 I = 1,NDECOM
              IF (WORK(I+IA-1) .EQ. 0.D0) GO TO 690
 340          WORK(I+ISAVE2-1) = WORK(I+ISAVE2-1)/WORK(I+IA-1)
          ELSE IF (IMPL .EQ. 3) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (NPAR, T, Y, WORK(IA), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGEFA (WORK(IA), MATDIM, NDE, IWORK(INDPVT), INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGESL (WORK(IA), MATDIM, NDE, IWORK(INDPVT),
     8                    WORK(ISAVE2), 0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              CALL FA (NPAR, T, Y, WORK(IA+ML), MATDIM, ML, MU, NDECOM)
              IF (NPAR .EQ. 0) THEN
                NSTATE = 9
                RETURN
              END IF
              CALL DGBFA (WORK(IA), MATDIM, NDE, ML, MU, IWORK(INDPVT),
     8                    INFO)
              IF (INFO .NE. 0) GO TO 690
              CALL DGBSL (WORK(IA), MATDIM, NDE, ML, MU, IWORK(INDPVT),
     8                    WORK(ISAVE2), 0)
            END IF
          END IF
        END IF
        DO 360 J = I,N
          IF (Y(J) .NE. 0.D0) THEN
            WORK(J+IYWT-1) = ABS(Y(J))
          ELSE
            IF (IWORK(IJTASK) .EQ. 0) THEN
              WORK(J+IYWT-1) = ABS(H*WORK(J+ISAVE2-1))
            ELSE
              WORK(J+IYWT-1) = ABS(WORK(J+IYH+N-1))
            END IF
          END IF
          IF (WORK(J+IYWT-1) .EQ. 0.D0) WORK(J+IYWT-1) = UROUND
 360      CONTINUE
      ELSE IF (IERROR .EQ. 3) THEN
        DO 380 I = 1,N
 380      WORK(I+IYWT-1) = MAX(EWT(1), ABS(Y(I)))
      ELSE IF (IERROR .EQ. 4) THEN
        DO 400 I = 1,N
 400      WORK(I+IYWT-1) = MAX(EWT(I), ABS(Y(I)))
      END IF
C
 410  DO 420 I = 1,N
 420    WORK(I+ISAVE2-1) = Y(I)/WORK(I+IYWT-1)
      SUM = DNRM2(N, WORK(ISAVE2), 1)/SQRT(DBLE(N))
      SUM = MAX(1.D0, SUM)
      IF (EPS .LT. SUM*UROUND) THEN
        EPS = SUM*UROUND*(1.D0 + 10.D0*UROUND)
        WRITE(RL1, '(D16.8)') T
        WRITE(RL2, '(D16.8)') EPS
        IERFLG = 4
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'At T, '//RL1//', the requested accuracy, EPS, was not '//
     8  'obtainable with the machine precision.  EPS has been '//
     8  'increased to '//RL2//' .', IERFLG, 0)
        NSTATE = 4
        GO TO 560
      END IF
      IF (ABS(H) .GE. UROUND*ABS(T)) THEN
        IWORK(INDPRT) = 0
      ELSE IF (IWORK(INDPRT) .EQ. 0) THEN
        WRITE(RL1, '(D16.8)') T
        WRITE(RL2, '(D16.8)') H
        IERFLG = 15
        CALL XERMSG('SLATEC', 'DDRIV3',
     8  'At T, '//RL1//', the step size, '//RL2//', is smaller '//
     8  'than the roundoff level of T.  This may occur if there is '//
     8  'an abrupt change in the right hand side of the '//
     8  'differential equations.', IERFLG, 0)
        IWORK(INDPRT) = 1
      END IF
      IF (NTASK.NE.2) THEN
        IF ((IWORK(INSTEP)-NSTEPL) .EQ. MXSTEP) THEN
          WRITE(RL1, '(D16.8)') T
          WRITE(INTGR1, '(I8)') MXSTEP
          WRITE(RL2, '(D16.8)') TOUT
          IERFLG = 3
          CALL XERMSG('SLATEC', 'DDRIV3',
     8    'At T, '//RL1//', '//INTGR1//' steps have been taken '//
     8    'without reaching TOUT, '//RL2//' .', IERFLG, 0)
          NSTATE = 3
          GO TO 560
        END IF
      END IF
C
C     CALL DDSTP (EPS, F, FA, HMAX, IMPL, IERROR, JACOBN, MATDIM,
C    8            MAXORD, MINT, MITER, ML, MU, N, NDE, YWT, UROUND,
C    8            USERS,  AVGH, AVGORD, H, HUSED, JTASK, MNTOLD, MTROLD,
C    8            NFE, NJE, NQUSED, NSTEP, T, Y, YH,  A, CONVRG,
C    8            DFDY, EL, FAC, HOLD, IPVT, JSTATE, JSTEPL, NQ, NWAIT,
C    8            RC, RMAX, SAVE1, SAVE2, TQ, TREND, ISWFLG, MTRSV,
C    8            MXRDSV)
C
      CALL DDSTP (EPS, F, FA, WORK(IHMAX), IMPL, IERROR, JACOBN,
     8            MATDIM, IWORK(IMXORD), IWORK(IMNT), IWORK(IMTR), ML,
     8            MU, NPAR, NDECOM, WORK(IYWT), UROUND, USERS,
     8            WORK(IAVGH), WORK(IAVGRD), WORK(IH), HUSED,
     8            IWORK(IJTASK), IWORK(IMNTLD), IWORK(IMTRLD),
     8            IWORK(INFE), IWORK(INJE), IWORK(INQUSE),
     8            IWORK(INSTEP), WORK(IT), Y, WORK(IYH), WORK(IA),
     8            CONVRG, WORK(IDFDY), WORK(IEL), WORK(IFAC),
     8            WORK(IHOLD), IWORK(INDPVT), JSTATE, IWORK(IJSTPL),
     8            IWORK(INQ), IWORK(INWAIT), WORK(IRC), WORK(IRMAX),
     8            WORK(ISAVE1), WORK(ISAVE2), WORK(ITQ), WORK(ITREND),
     8            MINT, IWORK(IMTRSV), IWORK(IMXRDS))
      T = WORK(IT)
      H = WORK(IH)
      IF (CONVRG) THEN
        IWORK(ICNVRG) = 1
      ELSE
        IWORK(ICNVRG) = 0
      END IF
      GO TO (470, 670, 680, 690, 690, 660, 660, 660, 660, 660), JSTATE
 470  IWORK(IJTASK) = 1
C                                 Determine if a root has been overtaken
      IF (NROOT .NE. 0) THEN
        IROOT = 0
        DO 500 I = 1,NROOT
          GLAST = WORK(I+IGNOW-1)
          GNOW = G (NPAR, T, Y, I)
          IF (NPAR .EQ. 0) THEN
            IWORK(INROOT) = I
            NSTATE = 7
            RETURN
          END IF
          WORK(I+IGNOW-1) = GNOW
          IF (GLAST*GNOW .GT. 0.D0) THEN
            WORK(I+ITROOT-1) = T + H
          ELSE
            IF (GNOW .EQ. 0.D0) THEN
              WORK(I+ITROOT-1) = T
              IROOT = I
            ELSE
              IF (GLAST .EQ. 0.D0) THEN
                WORK(I+ITROOT-1) = T + H
              ELSE
                IF (ABS(HUSED) .GE. UROUND*ABS(T)) THEN
                  TLAST = T - HUSED
                  IROOT = I
                  TROOT = T
                  CALL DDZRO (AE, G, H, NPAR, IWORK(INQ), IROOT, RE, T,
     8                        WORK(IYH), UROUND,  TROOT, TLAST,
     8                        GNOW, GLAST,  Y)
                  DO 480 J = 1,N
 480                Y(J) = WORK(IYH+J-1)
                  IF (NPAR .EQ. 0) THEN
                    IWORK(INROOT) = I
                    NSTATE = 7
                    RETURN
                  END IF
                  WORK(I+ITROOT-1) = TROOT
                ELSE
                  WORK(I+ITROOT-1) = T
                  IROOT = I
                END IF
              END IF
            END IF
          END IF
 500      CONTINUE
        IF (IROOT .EQ. 0) THEN
          IWORK(IJROOT) = 0
C                                                  Select the first root
        ELSE
          IWORK(IJROOT) = NTASK
          IWORK(INRTLD) = NROOT
          IWORK(INDTRT) = ITROOT
          TROOT = T + H
          DO 510 I = 1,NROOT
            IF (WORK(I+ITROOT-1)*HSIGN .LT. TROOT*HSIGN) THEN
              TROOT = WORK(I+ITROOT-1)
              IROOT = I
            END IF
 510        CONTINUE
          IWORK(INROOT) = IROOT
          WORK(ITOUT) = TROOT
          IF (TROOT*HSIGN .LE. TOUT*HSIGN) THEN
            CALL DDNTP (H, 0, N, IWORK(INQ), T, TROOT, WORK(IYH),  Y)
            NSTATE = 5
            T = TROOT
            IERFLG = 0
            GO TO 580
          END IF
        END IF
      END IF
C                               Test for NTASK condition to be satisfied
      NSTATE = 2
      IF (NTASK .EQ. 1) THEN
        IF (T*HSIGN .LT. TOUT*HSIGN) GO TO 260
        CALL DDNTP (H, 0, N, IWORK(INQ), T, TOUT, WORK(IYH),  Y)
        T = TOUT
        IERFLG = 0
        GO TO 580
C                               TOUT is assumed to have been attained
C                               exactly if T is within twenty roundoff
C                               units of TOUT, relative to MAX(TOUT, T).
C
      ELSE IF (NTASK .EQ. 2) THEN
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
        ELSE
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
            H = TOUT - T
            IF ((T + H)*HSIGN.GT.TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
            WORK(IH) = H
            IF (H .EQ. 0.D0) GO TO 670
            IWORK(IJTASK) = -1
          END IF
        END IF
      ELSE IF (NTASK .EQ. 3) THEN
        IF (ABS(TOUT - T).LE.NROUND*UROUND*MAX(ABS(T), ABS(TOUT))) THEN
          T = TOUT
        ELSE
          IF ((T + H)*HSIGN .GT. TOUT*HSIGN) THEN
            H = TOUT - T
            IF ((T + H)*HSIGN.GT.TOUT*HSIGN) H = H*(1.D0 - 4.D0*UROUND)
            WORK(IH) = H
            IF (H .EQ. 0.D0) GO TO 670
            IWORK(IJTASK) = -1
          END IF
          GO TO 260
        END IF
      END IF
      IERFLG = 0
C                                      All returns are made through this
C                                      section.  IMXERR is determined.
 560  DO 570 I = 1,N
 570    Y(I) = WORK(I+IYH-1)
 580  IF (IWORK(IJTASK) .EQ. 0) RETURN
      BIG = 0.D0
      IMXERR = 1
      DO  590 I = 1,N
C                                            SIZE = ABS(ERROR(I)/YWT(I))
        SIZE = ABS(WORK(I+ISAVE1-1)/WORK(I+IYWT-1))
        IF (BIG .LT. SIZE) THEN
          BIG = SIZE
          IMXERR = I
        END IF
 590    CONTINUE
      IWORK(INDMXR) = IMXERR
      WORK(IHUSED) = HUSED
      RETURN
C
 660  NSTATE = JSTATE
      RETURN
C                                        Fatal errors are processed here
C
 670  WRITE(RL1, '(D16.8)') T
      IERFLG = 41
      CALL XERMSG('SLATEC', 'DDRIV3',
     8  'At T, '//RL1//', the attempted step size has gone to '//
     8  'zero.  Often this occurs if the problem setup is incorrect.',
     8  IERFLG, 1)
      NSTATE = 12
      RETURN
C
 680  WRITE(RL1, '(D16.8)') T
      IERFLG = 42
      CALL XERMSG('SLATEC', 'DDRIV3',
     8  'At T, '//RL1//', the step size has been reduced about 50 '//
     8  'times without advancing the solution.  Often this occurs '//
     8  'if the problem setup is incorrect.', IERFLG, 1)
      NSTATE = 12
      RETURN
C
 690  WRITE(RL1, '(D16.8)') T
      IERFLG = 43
      CALL XERMSG('SLATEC', 'DDRIV3',
     8  'At T, '//RL1//', while solving A*YDOT = F, A is singular.',
     8  IERFLG, 1)
      NSTATE = 12
      RETURN
      END
