DECLARE FUNCTION BESSK1! (X!)
DECLARE FUNCTION BESSI1! (X!)
DATA 1.0D0,0.15443144D0,-0.67278579D0,-0.18156897D0
DATA -0.1919402D-1,-0.110404D-2,-0.4686D-4
DATA 1.25331414D0,0.23498619D0,-0.3655620D-1
DATA 0.1504268D-1,-0.780353D-2,0.325614D-2,-0.68245D-3

FUNCTION BESSK1 (X)
RESTORE
READ P1#, P2#, P3#, P4#, P5#, P6#, P7#
READ Q1#, Q2#, Q3#, Q4#, Q5#, Q6#, Q7#
IF X <= 2! THEN
  Y# = X * X / 4!
  DUM# = P2# + Y# * (P3# + Y# * (P4# + Y# * (P5# + Y# * (P6# + Y# * P7#))))
  BESSK1 = (LOG(X / 2!) * BESSI1(X)) + (1! / X) * (P1# + Y# * DUM#)
ELSE
  Y# = 2! / X
  DUM# = Q2# + Y# * (Q3# + Y# * (Q4# + Y# * (Q5# + Y# * (Q6# + Y# * Q7#))))
  BESSK1 = (EXP(-X) / SQR(X)) * (Q1# + Y# * DUM#)
END IF
END FUNCTION

