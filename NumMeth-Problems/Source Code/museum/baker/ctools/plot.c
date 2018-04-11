/* simple line printer plotting package

from Handbook of C tools for Scientists and Engineers by L. Baker

DEPENDENCIES: none
*/
/*
#include "libc.h"
*/
#define max(a,b) (((a)>(b))? (a):(b))
#define min(a,b) (((a)<(b))? (a):(b))

#define NLINES 20

main(argc,argv) int argc;char **argv;
{
float y[100],x[100],z;int i,j;
for(i=0;i<40;i++)
	{
	x[i]=i;y[i]=i*i;
	}
splot(x,y,40);
exit(0);
}


setl(line,n,chr)
char chr,*line; int n;
{
int i;
for (i=0;i<n;i++) line[i]=chr;
return;
}

plotl(line,n) char line[];int n;
{
line[n]='\000';
printf("%s\n",line);
return;
}

splot(x,y,n) float x[],y[];int n;
{
float xmax,ymax,xmin,ymin,dx,dy,ytop,ybtm,ybar,dxi;
int k,i,j;
char line[81],label[17];
xmin=xmax=x[0];
ymin=ymax=y[0];
for(i=1;i<n;i++)
	{
	xmin= min(xmin,x[i]);
	xmax= max(xmax,x[i]);
	ymin= min(ymin,y[i]);
	ymax= max(ymax,y[i]);
	}

dy=(ymax-ymin)/NLINES;
dx=(xmax-xmin);
dxi=50./dx;
if( dx==0 || dy==0)
	{printf(" plot range=%f domain=%f\n",dx,dy);
		return;
	}

setl(label,19,' ');
label[18]='\000';/* make sure label is null terminated*/

ytop=ymax;
ybtm=ytop-dy;
for (i=0;i<NLINES;i++)
	{if(i<(NLINES-1))setl(line,80,' ');
	 else setl(line,80,'-');
	 line[19]='|';
	for(j=0;j<n;j++)
		{
		if( y[j]>ybtm && y[j]<=ytop)
			{k=(x[j]-xmin)*dxi+19;
			 line[k ]='X';

			}/*if*/
		};/*for*/
if(i==0){sprintf(label,"%10.2f",ymax);
	strncpy(line,label,10);
	unclob(line,12);
	}
else if (i==(NLINES-1))
	{sprintf(label,"%10.2f",ymin);
	strncpy(line,label,10);
	unclob(line,12);
	};
	plotl(line,78);
	ytop-=dy;
	ybtm-=dy;
	}/* end of loop over lines*/
setl(line,80,' ');
/* x values print*/
sprintf(label,"%10.2f",xmin );strncpy(&(line[15]),label,10);
sprintf(label,"%10.2f",xmax);strncpy(&(line[65]),label,10);
plotl(line,78);
return;
}

unclob(line,kt) int kt; char line[];
{int i;
for (i=0;i<=kt;i++)
{
if(line[i]=='\000')line[i]=' ';
}

}

