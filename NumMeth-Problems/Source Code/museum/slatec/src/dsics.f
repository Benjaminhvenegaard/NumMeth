      SUBROUTINE DSICS (N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, EL, D,
     +   R, IWARN)
C     .. Scalar Arguments ..
      INTEGER ISYM, IWARN, N, NEL, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), D(N), EL(NEL), R(N)
      INTEGER IA(NELT), IEL(NEL), JA(NELT), JEL(NEL)
C     .. Local Scalars ..
      DOUBLE PRECISION ELTMP
      INTEGER I, IBGN, IC, ICBGN, ICEND, ICOL, IEND, IR, IRBGN, IREND,
     +        IROW, IRR, J, JBGN, JELTMP, JEND
      CHARACTER XERN1*8
C     .. External Subroutines ..
      EXTERNAL XERMSG
C***FIRST EXECUTABLE STATEMENT  DSICS
C
C         Set the lower triangle in IEL, JEL, EL
C
      IWARN = 0
C
C         All matrix elements stored in IA, JA, A.  Pick out the lower
C         triangle (making sure that the Diagonal of EL is one) and
C         store by rows.
C
      NEL = 1
      IEL(1) = 1
      JEL(1) = 1
      EL(1) = 1
      D(1) = A(1)
CVD$R NOCONCUR
      DO 30 IROW = 2, N
C         Put in the Diagonal.
         NEL = NEL + 1
         IEL(IROW) = NEL
         JEL(NEL) = IROW
         EL(NEL) = 1
         D(IROW) = A(JA(IROW))
C
C         Look in all the lower triangle columns for a matching row.
C         Since the matrix is symmetric, we can look across the
C         IROW-th row by looking down the IROW-th column (if it is
C         stored ISYM=0)...
         IF( ISYM.EQ.0 ) THEN
            ICBGN = JA(IROW)
            ICEND = JA(IROW+1)-1
         ELSE
            ICBGN = 1
            ICEND = IROW-1
         ENDIF
         DO 20 IC = ICBGN, ICEND
            IF( ISYM.EQ.0 ) THEN
               ICOL = IA(IC)
               IF( ICOL.GE.IROW ) GOTO 20
            ELSE
               ICOL = IC
            ENDIF
            JBGN = JA(ICOL)+1
            JEND = JA(ICOL+1)-1
            IF( JBGN.LE.JEND .AND. IA(JEND).GE.IROW ) THEN
CVD$ NOVECTOR
               DO 10 J = JBGN, JEND
                  IF( IA(J).EQ.IROW ) THEN
                     NEL = NEL + 1
                     JEL(NEL) = ICOL
                     EL(NEL)  = A(J)
                     GOTO 20
                  ENDIF
 10            CONTINUE
            ENDIF
 20      CONTINUE
 30   CONTINUE
      IEL(N+1) = NEL+1
C
C         Sort ROWS of lower triangle into descending order (count out
C         along rows out from Diagonal).
C
      DO 60 IROW = 2, N
         IBGN = IEL(IROW)+1
         IEND = IEL(IROW+1)-1
         IF( IBGN.LT.IEND ) THEN
            DO 50 I = IBGN, IEND-1
CVD$ NOVECTOR
               DO 40 J = I+1, IEND
                  IF( JEL(I).GT.JEL(J) ) THEN
                     JELTMP = JEL(J)
                     JEL(J) = JEL(I)
                     JEL(I) = JELTMP
                     ELTMP = EL(J)
                     EL(J) = EL(I)
                     EL(I) = ELTMP
                  ENDIF
 40            CONTINUE
 50         CONTINUE
         ENDIF
 60   CONTINUE
C
C         Perform the Incomplete Cholesky decomposition by looping
C         over the rows.
C         Scale the first column.  Use the structure of A to pick out
C         the rows with something in column 1.
C
      IRBGN = JA(1)+1
      IREND = JA(2)-1
      DO 65 IRR = IRBGN, IREND
         IR = IA(IRR)
C         Find the index into EL for EL(1,IR).
C         Hint: it's the second entry.
         I = IEL(IR)+1
         EL(I) = EL(I)/D(1)
 65   CONTINUE
C
      DO 110 IROW = 2, N
C
C         Update the IROW-th diagonal.
C
         DO 66 I = 1, IROW-1
            R(I) = 0
 66      CONTINUE
         IBGN = IEL(IROW)+1
         IEND = IEL(IROW+1)-1
         IF( IBGN.LE.IEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
            DO 70 I = IBGN, IEND
               R(JEL(I)) = EL(I)*D(JEL(I))
               D(IROW) = D(IROW) - EL(I)*R(JEL(I))
 70         CONTINUE
C
C         Check to see if we have a problem with the diagonal.
C
            IF( D(IROW).LE.0.0D0 ) THEN
               IF( IWARN.EQ.0 ) IWARN = IROW
               D(IROW) = 1
            ENDIF
         ENDIF
C
C         Update each EL(IROW+1:N,IROW), if there are any.
C         Use the structure of A to determine the Non-zero elements
C         of the IROW-th column of EL.
C
         IRBGN = JA(IROW)
         IREND = JA(IROW+1)-1
         DO 100 IRR = IRBGN, IREND
            IR = IA(IRR)
            IF( IR.LE.IROW ) GOTO 100
C         Find the index into EL for EL(IR,IROW)
            IBGN = IEL(IR)+1
            IEND = IEL(IR+1)-1
            IF( JEL(IBGN).GT.IROW ) GOTO 100
            DO 90 I = IBGN, IEND
               IF( JEL(I).EQ.IROW ) THEN
                  ICEND = IEND
 91               IF( JEL(ICEND).GE.IROW ) THEN
                     ICEND = ICEND - 1
                     GOTO 91
                  ENDIF
C         Sum up the EL(IR,1:IROW-1)*R(1:IROW-1) contributions.
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
                  DO 80 IC = IBGN, ICEND
                     EL(I) = EL(I) - EL(IC)*R(JEL(IC))
 80               CONTINUE
                  EL(I) = EL(I)/D(IROW)
                  GOTO 100
               ENDIF
 90         CONTINUE
C
C         If we get here, we have real problems...
            WRITE (XERN1, '(I8)') IROW
            CALL XERMSG ('SLATEC', 'DSICS',
     $         'A and EL data structure mismatch in row '// XERN1, 1, 2)
 100     CONTINUE
 110  CONTINUE
C
C         Replace diagonals by their inverses.
C
CVD$ CONCUR
      DO 120 I =1, N
         D(I) = 1.0D0/D(I)
 120  CONTINUE
      RETURN
C------------- LAST LINE OF DSICS FOLLOWS ----------------------------
      END
