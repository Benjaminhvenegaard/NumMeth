      SUBROUTINE SBHIN (N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB)
C     .. Scalar Arguments ..
      INTEGER ISYM, IUNIT, JOB, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), RHS(N), SOLN(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      REAL TEMP
      INTEGER I, IBGN, ICOL, IEND, ITEMP, J, JOBRET, NCOL, NELE, NIND,
     +        NLINE, NNVLS, NPLS, NRHSLS, NRILS, NROW
      CHARACTER CODE*3, PNTFMT*16, RINFMT*16, NVLFMT*20, RHSFMT*20,
     +          TITLE*80
C     .. Intrinsic Functions ..
      INTRINSIC MOD
C***FIRST EXECUTABLE STATEMENT  SBHIN
C
C         Read Matrices In BOEING-HARWELL format.
C
C TITLE  Header line to identify data file.
C NLINE  Number of data lines (after the header) in the file.
C NPLS   Number of lines for the Column Pointer data in the file.
C NRILS  Number of lines for the Row indices in the data file.
C NNVLS  Number of lines for the Matrix elements in the data file.
C NRHSLS Number of lines for the RHS in the data file.
C ---- Only those variables needed by SLAP are referenced. ----
C
      READ(IUNIT,9000) TITLE
      READ(IUNIT,9010) NLINE, NPLS, NRILS, NNVLS, NRHSLS
      READ(IUNIT,9020) CODE, NROW, NCOL, NIND, NELE
      READ(IUNIT,9030) PNTFMT, RINFMT, NVLFMT, RHSFMT
C
      IF( NROW.GT.N ) THEN
         N = NROW
         JOBRET = -1
         GOTO 999
      ENDIF
      IF( NIND.GT.NELT ) THEN
         NELT = NIND
         JOBRET = -2
         GOTO 999
      ENDIF
C
C         Set the parameters.
C
      N    = NROW
      NELT = NIND
      IF( CODE.EQ.'RUA' ) THEN
         ISYM = 0
      ELSE IF( CODE.EQ.'RSA' ) THEN
         ISYM = 1
      ELSE
         JOBRET = -3
         GOTO 999
      ENDIF
      READ(IUNIT,PNTFMT) (JA(I), I = 1, N+1)
      READ(IUNIT,RINFMT) (IA(I), I = 1, NELT)
      JOBRET = 10
      IF( NNVLS.GT.0 ) THEN
         READ(IUNIT,NVLFMT) (A(I),  I = 1, NELT)
         JOBRET = 0
      ENDIF
      IF( MOD(JOB,2).EQ.1 ) THEN
C
C         User requests that the RHS be read in.  If it is in the input
C         file, read it in; otherwise just zero it out.
C
         IF( NRHSLS.GT.0 ) THEN
            READ(5,RHSFMT) (RHS(I), I = 1, N)
            JOBRET = JOBRET + 1
         ELSE
            DO 10 I = 1, N
               RHS(I) = 0
 10         CONTINUE
         ENDIF
      ENDIF
      IF ( (JOB.EQ.2).OR.(JOB.EQ.3) ) THEN
C
C         User requests that the SOLN be read in.
C         Just zero out the array.
C
         DO 20 I = 1, N
            SOLN(I) = 0
 20      CONTINUE
      ENDIF
C
C         Now loop through the IA array making sure that the diagonal
C         matrix element appears first in the column.  Then sort the
C         rest of the column in ascending order.
C
CVD$R NOCONCUR
CVD$R NOVECTOR
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
C
C         Set return flag.
 999  JOB = JOBRET
      RETURN
 9000 FORMAT( A80 )
 9010 FORMAT( 5I14 )
 9020 FORMAT( A3, 11X, 4I14 )
 9030 FORMAT( 2A16, 2A20 )
C------------- LAST LINE OF SBHIN FOLLOWS ------------------------------
      END
