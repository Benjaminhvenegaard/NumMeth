      SUBROUTINE CDNTL (EPS, F, FA, HMAX, HOLD, IMPL, JTASK, MATDIM,
     8   MAXORD, MINT, MITER, ML, MU, N, NDE, SAVE1, T, UROUND, USERS,
     8   Y, YWT, H, MNTOLD, MTROLD, NFE, RC, YH, A, CONVRG, EL, FAC,
     8   IER, IPVT, NQ, NWAIT, RH, RMAX, SAVE2, TQ, TREND, ISWFLG,
     8   JSTATE)
      INTEGER I, IFLAG, IMPL, INFO, ISWFLG, JSTATE, JTASK, MATDIM,
     8        MAXORD, MINT, MITER, ML, MNTOLD, MTROLD, MU, N, NDE, NFE,
     8        NQ, NWAIT
      COMPLEX A(MATDIM,*), FAC(*), SAVE1(*), SAVE2(*), Y(*), YH(N,*),
     8        YWT(*)
      REAL EL(13,12), EPS, H, HMAX, HOLD, OLDL0, RC, RH, RMAX,
     8     RMINIT, SCNRM2, SUM, T, TQ(3,12), TREND, UROUND
      INTEGER IPVT(*)
      LOGICAL CONVRG, IER
      PARAMETER(RMINIT = 10000.E0)
C***FIRST EXECUTABLE STATEMENT  CDNTL
      IER = .FALSE.
      IF (JTASK .GE. 0) THEN
        IF (JTASK .EQ. 0) THEN
          CALL CDCST (MAXORD, MINT, ISWFLG,  EL, TQ)
          RMAX = RMINIT
        END IF
        RC = 0.E0
        CONVRG = .FALSE.
        TREND = 1.E0
        NQ = 1
        NWAIT = 3
        CALL F (N, T, Y, SAVE2)
        IF (N .EQ. 0) THEN
          JSTATE = 6
          RETURN
        END IF
        NFE = NFE + 1
        IF (IMPL .NE. 0) THEN
          IF (MITER .EQ. 3) THEN
            IFLAG = 0
            CALL USERS (Y, YH, YWT, SAVE1, SAVE2, T, H, EL, IMPL, N,
     8                  NDE, IFLAG)
            IF (IFLAG .EQ. -1) THEN
              IER = .TRUE.
              RETURN
            END IF
            IF (N .EQ. 0) THEN
              JSTATE = 10
              RETURN
            END IF
          ELSE IF (IMPL .EQ. 1) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL CGEFA (A, MATDIM, N, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL CGESL (A, MATDIM, N, IPVT, SAVE2, 0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              CALL FA (N, T, Y, A(ML+1,1), MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL CGBFA (A, MATDIM, N, ML, MU, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL CGBSL (A, MATDIM, N, ML, MU, IPVT, SAVE2, 0)
            END IF
          ELSE IF (IMPL .EQ. 2) THEN
            CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
            IF (N .EQ. 0) THEN
              JSTATE = 9
              RETURN
            END IF
            DO 150 I = 1,NDE
              IF (A(I,1) .EQ. 0.E0) THEN
                IER = .TRUE.
                RETURN
              ELSE
                SAVE2(I) = SAVE2(I)/A(I,1)
              END IF
 150          CONTINUE
            DO 155 I = NDE+1,N
 155          A(I,1) = 0.E0
          ELSE IF (IMPL .EQ. 3) THEN
            IF (MITER .EQ. 1 .OR. MITER .EQ. 2) THEN
              CALL FA (N, T, Y, A, MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL CGEFA (A, MATDIM, NDE, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL CGESL (A, MATDIM, NDE, IPVT, SAVE2, 0)
            ELSE IF (MITER .EQ. 4 .OR. MITER .EQ. 5) THEN
              CALL FA (N, T, Y, A(ML+1,1), MATDIM, ML, MU, NDE)
              IF (N .EQ. 0) THEN
                JSTATE = 9
                RETURN
              END IF
              CALL CGBFA (A, MATDIM, NDE, ML, MU, IPVT, INFO)
              IF (INFO .NE. 0) THEN
                IER = .TRUE.
                RETURN
              END IF
              CALL CGBSL (A, MATDIM, NDE, ML, MU, IPVT, SAVE2, 0)
            END IF
          END IF
        END IF
        DO 170 I = 1,NDE
 170      SAVE1(I) = SAVE2(I)/MAX(1.E0, ABS(YWT(I)))
        SUM = SCNRM2(NDE, SAVE1, 1)/SQRT(REAL(NDE))
        IF (SUM .GT. EPS/ABS(H)) H = SIGN(EPS/SUM, H)
        DO 180 I = 1,N
 180      YH(I,2) = H*SAVE2(I)
        IF (MITER .EQ. 2 .OR. MITER .EQ. 5 .OR. ISWFLG .EQ. 3) THEN
          DO 20 I = 1,N
 20         FAC(I) = SQRT(UROUND)
        END IF
      ELSE
        IF (MITER .NE. MTROLD) THEN
          MTROLD = MITER
          RC = 0.E0
          CONVRG = .FALSE.
        END IF
        IF (MINT .NE. MNTOLD) THEN
          MNTOLD = MINT
          OLDL0 = EL(1,NQ)
          CALL CDCST (MAXORD, MINT, ISWFLG,  EL, TQ)
          RC = RC*EL(1,NQ)/OLDL0
          NWAIT = NQ + 2
        END IF
        IF (H .NE. HOLD) THEN
          NWAIT = NQ + 2
          RH = H/HOLD
          CALL CDSCL (HMAX, N, NQ, RMAX,  HOLD, RC, RH, YH)
        END IF
      END IF
      RETURN
      END
