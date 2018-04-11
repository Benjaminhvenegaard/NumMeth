      SUBROUTINE CSHCH (Z, CSH, CCH)
      COMPLEX CCH, CSH, Z
      REAL CCHI, CCHR, CH, CN, CSHI, CSHR, SH, SN, X, Y
C***FIRST EXECUTABLE STATEMENT  CSHCH
      X = REAL(Z)
      Y = AIMAG(Z)
      SH = SINH(X)
      CH = COSH(X)
      SN = SIN(Y)
      CN = COS(Y)
      CSHR = SH*CN
      CSHI = CH*SN
      CSH = CMPLX(CSHR,CSHI)
      CCHR = CH*CN
      CCHI = SH*SN
      CCH = CMPLX(CCHR,CCHI)
      RETURN
      END
