      INTEGER FUNCTION I1MACH (I)
C
      INTEGER IMACH(16),OUTPUT
      SAVE IMACH
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR THE AMIGA
C     ABSOFT COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE APOLLO
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        129 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1025 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM
C
C     DATA IMACH( 1) /          7 /
C     DATA IMACH( 2) /          2 /
C     DATA IMACH( 3) /          2 /
C     DATA IMACH( 4) /          2 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         33 /
C     DATA IMACH( 9) / Z1FFFFFFFF /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -256 /
C     DATA IMACH(13) /        255 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /       -256 /
C     DATA IMACH(16) /        255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /        -50 /
C     DATA IMACH(16) /         76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         48 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         39 /
C     DATA IMACH( 9) / O0007777777777777 /
C     DATA IMACH(10) /          8 /
C     DATA IMACH(11) /         13 /
C     DATA IMACH(12) /        -50 /
C     DATA IMACH(13) /         76 /
C     DATA IMACH(14) /         26 /
C     DATA IMACH(15) /     -32754 /
C     DATA IMACH(16) /      32780 /
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -4095 /
C     DATA IMACH(13) /       4094 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -4095 /
C     DATA IMACH(16) /       4094 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /    6LOUTPUT/
C     DATA IMACH( 5) /         60 /
C     DATA IMACH( 6) /         10 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         48 /
C     DATA IMACH( 9) / 00007777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /       -929 /
C     DATA IMACH(13) /       1070 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /       -929 /
C     DATA IMACH(16) /       1069 /
C
C     MACHINE CONSTANTS FOR THE CELERITY C1260
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / Z'7FFFFFFF' /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fn COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -fi COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -p8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16383 /
C     DATA IMACH(16) /      16383 /
C
C     MACHINE CONSTANTS FOR THE CONVEX
C     USING THE -pd8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 9223372036854775807 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1023 /
C     DATA IMACH(13) /       1023 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 46 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         46 /
C     DATA IMACH( 9) / 1777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE CRAY
C     USING THE 64 BIT INTEGER COMPILER OPTION
C
C     DATA IMACH( 1) /        100 /
C     DATA IMACH( 2) /        101 /
C     DATA IMACH( 3) /        102 /
C     DATA IMACH( 4) /        101 /
C     DATA IMACH( 5) /         64 /
C     DATA IMACH( 6) /          8 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         63 /
C     DATA IMACH( 9) / 777777777777777777777B /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         47 /
C     DATA IMACH(12) /      -8189 /
C     DATA IMACH(13) /       8190 /
C     DATA IMACH(14) /         94 /
C     DATA IMACH(15) /      -8099 /
C     DATA IMACH(16) /       8190 /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     DATA IMACH( 1) /         11 /
C     DATA IMACH( 2) /         12 /
C     DATA IMACH( 3) /          8 /
C     DATA IMACH( 4) /         10 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING G_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE DEC ALPHA
C     USING IEEE_FLOAT
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC RISC
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING D_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE DEC VAX
C     USING G_FLOATING
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1023 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE ELXSI 6400
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1022 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         24 /
C     DATA IMACH( 6) /          3 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         23 /
C     DATA IMACH( 9) /    8388607 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         38 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /         43 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          6 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         63 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 730
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         39 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          4 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         23 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         55 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE HP 9000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          7 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         32 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -126 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1015 /
C     DATA IMACH(16) /       1017 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE PERKIN ELMER (INTERDATA) 7/32.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          7 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) /  Z7FFFFFFF /
C     DATA IMACH(10) /         16 /
C     DATA IMACH(11) /          6 /
C     DATA IMACH(12) /        -64 /
C     DATA IMACH(13) /         63 /
C     DATA IMACH(14) /         14 /
C     DATA IMACH(15) /        -64 /
C     DATA IMACH(16) /         63 /
C
C     MACHINE CONSTANTS FOR THE IBM PC
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE IBM RS 6000
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          0 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE INTEL i860
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         54 /
C     DATA IMACH(15) /       -101 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR)
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          5 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / "377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         62 /
C     DATA IMACH(15) /       -128 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          5 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C     MACHINE CONSTANTS FOR THE SILICON GRAPHICS
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -125 /
C     DATA IMACH(13) /        128 /
C     DATA IMACH(14) /         53 /
C     DATA IMACH(15) /      -1021 /
C     DATA IMACH(16) /       1024 /
C
C     MACHINE CONSTANTS FOR THE SUN
C     USING THE -r8 COMPILER OPTION
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          6 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         32 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         31 /
C     DATA IMACH( 9) / 2147483647 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         53 /
C     DATA IMACH(12) /      -1021 /
C     DATA IMACH(13) /       1024 /
C     DATA IMACH(14) /        113 /
C     DATA IMACH(15) /     -16381 /
C     DATA IMACH(16) /      16384 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES FTN COMPILER
C
C     DATA IMACH( 1) /          5 /
C     DATA IMACH( 2) /          6 /
C     DATA IMACH( 3) /          1 /
C     DATA IMACH( 4) /          6 /
C     DATA IMACH( 5) /         36 /
C     DATA IMACH( 6) /          4 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         35 /
C     DATA IMACH( 9) / O377777777777 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         27 /
C     DATA IMACH(12) /       -128 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         60 /
C     DATA IMACH(15) /      -1024 /
C     DATA IMACH(16) /       1023 /
C
C     MACHINE CONSTANTS FOR THE Z80 MICROPROCESSOR
C
C     DATA IMACH( 1) /          1 /
C     DATA IMACH( 2) /          1 /
C     DATA IMACH( 3) /          0 /
C     DATA IMACH( 4) /          1 /
C     DATA IMACH( 5) /         16 /
C     DATA IMACH( 6) /          2 /
C     DATA IMACH( 7) /          2 /
C     DATA IMACH( 8) /         15 /
C     DATA IMACH( 9) /      32767 /
C     DATA IMACH(10) /          2 /
C     DATA IMACH(11) /         24 /
C     DATA IMACH(12) /       -127 /
C     DATA IMACH(13) /        127 /
C     DATA IMACH(14) /         56 /
C     DATA IMACH(15) /       -127 /
C     DATA IMACH(16) /        127 /
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16) GO TO 10
C
      I1MACH = IMACH(I)
      RETURN
C
   10 CONTINUE
      WRITE (UNIT = OUTPUT, FMT = 9000)
 9000 FORMAT ('1ERROR    1 IN I1MACH - I OUT OF BOUNDS')
C
C     CALL FDUMP
C
      STOP
      END
