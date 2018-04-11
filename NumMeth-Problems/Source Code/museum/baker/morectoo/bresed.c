/* version of Bresenham's line-drawing algorithm
            optimized for speed

from More C tools for scientists and engineers by L. Baker

*/
#include <stdio.h>
#define abs(x) ((x)>=0 ? (x) : -(x) )
extern xsize,ysize,diagnose;

main(argc,argv) int argc;char **argv;
{
/*test Breese*/
int c;
xsize=20;ysize=20;diagnose=1;
test(10,0,10,20);/*vertical*/
c=getchar();
test(0,10,20,10);/*horizontal*/
c=getchar();
test(0,0,20,20);/* 45 degree*/
c=getchar();
test(0,20,20,0);/*-45*/
c=getchar();
test(0,20,4,0);
c=getchar();
test(0,20,20,4);
c=getchar();
test(20,20,0,0);
c=getchar();
test(0,0,20,4);/* slope<1*/
c=getchar();
test(0,0,4,20);/* slope>1*/
}


test(x1,y1,x2,y2) int x1,y1,x2,y2;
{cleara();
draw(x1,y1,x2,y2,'X');
show();
}

