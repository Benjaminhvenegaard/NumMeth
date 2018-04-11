      SUBROUTINE INXCC (I, IR, IDXC, NC)
      COMMON /CCBLK/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
C***FIRST EXECUTABLE STATEMENT  INXCC
      NC = 2**IR
      IDXC = I
      IF (IDXC+NC-1-NM) 102,102,101
  101 NC = 0
  102 RETURN
      END
