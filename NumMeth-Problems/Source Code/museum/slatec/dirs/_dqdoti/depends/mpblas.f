      SUBROUTINE MPBLAS (I1)
      COMMON /MPCOM/ MPB, MPT, MPM, MPLUN, MPMXR, MPR(30)
C***FIRST EXECUTABLE STATEMENT  MPBLAS
      I1 = 1
C
C     For full extended precision accuracy, MPB should be as large as
C     possible, subject to the restrictions in Brent's paper.
C
C     Statements below are for an integer wordlength of  48, 36, 32,
C     24, 18, and 16.  Pick one, or generate a new one.
C       48     MPB = 4194304
C       36     MPB =   65536
C       32     MPB =   16384
C       24     MPB =    1024
C       18     MPB =     128
C       16     MPB =      64
C
      MPBEXP = I1MACH(8)/2-2
      MPB = 2**MPBEXP
C
C     Set up remaining parameters
C                  UNIT FOR ERROR MESSAGES
      MPLUN = I1MACH(4)
C                  NUMBER OF MP DIGITS
      MPT = (2*I1MACH(14)+MPBEXP-1)/MPBEXP
C                  DIMENSION OF R
      MPMXR = MPT+4
C
      if (MPMXR.GT.30) THEN
         CALL XERMSG('SLATEC', 'MPBLAS',
     *      'Array space not sufficient for Quad Precision 2x ' //
     *      'Double Precision, Proceeding.', 1, 1)
         MPT = 26
         MPMXR = 30
      ENDIF
C                  EXPONENT RANGE
      MPM = MIN(32767,I1MACH(9)/4-1)
      RETURN
      END
