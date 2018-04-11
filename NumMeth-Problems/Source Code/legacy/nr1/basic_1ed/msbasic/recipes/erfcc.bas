DECLARE FUNCTION ERFCC! (X!)

FUNCTION ERFCC (X)
Z = ABS(X)
T = 1! / (1! + .5 * Z)
DUM = T * (-.82215223# + T * .17087277#)
DUM = T * (-1.13520398# + T * (1.48851587# + DUM))
DUM = T * (.09678418# + T * (-.18628806# + T * (.27886807# + DUM)))
DUM = -Z * Z - 1.26551223# + T * (1.00002368# + T * (.37409196# + DUM))
DUM = T * EXP(DUM)
IF X < 0! THEN
  ERFCC = 2! - DUM
ELSE
  ERFCC = DUM
END IF
END FUNCTION
