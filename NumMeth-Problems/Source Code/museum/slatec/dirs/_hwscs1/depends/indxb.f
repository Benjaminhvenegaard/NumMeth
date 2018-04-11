      SUBROUTINE INDXB (I, IR, IDX, IDP)
C
      COMMON /CBLKT/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
C***FIRST EXECUTABLE STATEMENT  INDXB
      IDX = I
      IDP = 0
      IF (IR) 107,101,103
  101 IF (I-NM) 102,102,107
  102 IDX = I
      IDP = 1
      RETURN
  103 IZH = 2**IR
      ID = I-IZH-IZH
      IDX = ID+ID+(IR-1)*IK+IR+(IK-I)/IZH+4
      IPL = IZH-1
      IDP = IZH+IZH-1
      IF (I-IPL-NM) 105,105,104
  104 IDP = 0
      RETURN
  105 IF (I+IPL-NM) 107,107,106
  106 IDP = NM+IPL-I+1
  107 RETURN
      END
