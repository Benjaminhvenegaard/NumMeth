/* version of Bresenham's line-drawing algorithm
            optimized for speed

from More C tools for scientists and engineers by L. Baker

*/

/*
#include "libc.h"
*/

#define abs(x) ((x)>=0 ? (x) : -(x) )

#define XSIZE 50
#define YSIZE 50

int xsize= XSIZE, ysize=YSIZE, diagnose=0;
char screen[ XSIZE ][ YSIZE ];

plot(x,y,z) int x,y;char z;
{
if( x< xsize  && y < ysize && x>=0 && y>=0)screen[x][y]=z;
if(diagnose) printf(" x %d y %d plotted\n",x,y);
return;
}

show()
{
int i,j;
for(j=0;j< xsize;j++)
{ printf("\n");
for(i=0;i< ysize ;i++)printf("%c",screen[i][j]);}
printf("\n");
}


cleara()
{int i,j;
for (i=0;i< xsize ;i++)
	{for(j=0;j< ysize;j++) screen[i][j]=' ';}
}

draw (x1,y1,x2,y2,symbol)
int x1,y1,x2,y2;char symbol;
{/*Bresenham line draw consult Foley & van Dam for algorithm
 this version based on code by N. Barkakati, corrected by
  J. Mente, and improved by pulling an if test out of
  the while loops (at the cost of 4 not 2 while loops in code
  additional optimization attempted through the use of
  register and static variables and shifts instead of multiplies as
  in the first version of this method*/
static int dx,dy,incr1,incr2,incr3,end;
register x,y,d;
dx=abs(x2-x1);
dy=abs(y2-y1);
if (dx>dy)
{/* slope abs value less than 1 march in x */
if (x1>x2)
	{
	x=x2;
	y=y2;
	end=x1;
	dy=y1-y2;
	}
else
	{
	x=x1;
	y=y1;
	end=x2;
	dy=y2-y1;
	};

incr1=dy<<1;
incr2=(dy-dx)<<1;
incr3=(dy+dx)<<1;
d= (dy>=0)?incr1-dx:incr1+dx;

plot(x,y,symbol);

if(dy>=0)
{

while (x< end)
	{
	x++;
	if (d<0)
		{		
		d+=incr1;
		}
	else /*d>=0*/
		{
			y++;
			d+=incr2;/*was dy>0*/
		};
	plot(x,y,symbol);	
	}

}/*dy>=0*/
else
{/*dy<0*/

while (x< end)
	{
	x++;
	if (d<0)
		{		
			y--;
			d+=incr3;
		}
	else /*d>=0*/
		{
		d+=incr1;/*was dy<0 */
		};
	plot(x,y,symbol);	
	}

}/*dy<0*/

}/*dx>=dy*/
else
{/*dx<dy*/
/* slope greater than 1 interchange role of x&y*/
if (y1>y2)
	{
	x=x2;
	y=y2;
	end=y1;
	dx=x1-x2;
	}
else
	{
	x=x1;
	y=y1;
	end=y2;
	dx=x2-x1;
	};

incr1=dx<<1;
incr2=(dx-dy)<<1;
incr3=(dx+dy)<<1;
d=(dx>=0)? incr1-dy:incr1+dy;
plot(x,y,symbol);


if(dx<0)
{
while (y< end)
	{
	y++;
	if (d<0)
		{/* WAS dx<=0*/
			x--;
			d+=incr3;
		}
	else /* d>=0 */
		{
			d+=incr1;
		};
	plot(x,y,symbol);	
	}
}/*dx<0*/
else
{/*dx>=0*/

while (y< end)
	{
	y++;
	if (d<0)
		{
		d+=incr1;
		}
	else /* d>=0 */
		{/*was dx>0*/
			x++;
			d+=incr2;
		};
	plot(x,y,symbol);	
	}

}/* else dx>=0*/
}/* dx<dy end else*/
return;
}


