DECLARE SUB SORT (N!, RA!())
DECLARE FUNCTION ERF! (Y!)
DECLARE FUNCTION PROBKS! (ALAM!)
DECLARE FUNCTION FUNC! (X!)

SUB KSONE (DATQ(), N, DUM, D, PROB)
CALL SORT(N, DATQ())
EN = N
D = 0!
FO = 0!
FOR J = 1 TO N
  FQ = J / EN
  FF = FUNC(DATQ(J))
  DT = ABS(FO - FF)
  IF ABS(FQ - FF) > DT THEN DT = ABS(FQ - FF)
  IF DT > D THEN D = DT
  FO = FQ
NEXT J
PROB = PROBKS(SQR(EN) * D)
END SUB
