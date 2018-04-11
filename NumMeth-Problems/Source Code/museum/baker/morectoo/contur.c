/*
contour plotting program designed to plot
2d data in file PLOT.DAT 

from More C tools for scientists and engineers by L. Baker

*/

#define DOFOR(i,to) for(i=0;i<to;i++)
#define INDEX(i,j) [(j)+(i)*coln]
#define abs(x) ((x)>=0 ? (x) : -(x) )
#define sgn(x) ((x)>=0 ? 1 : -1 )
#define max(a,b) ( (a)>(b)? (a):(b))
#define min(a,b) ((a)<(b) ? (a):(b))
#define BLANK 32
#define NULL '\000'
/* =' ' didn't seen to work?*/
/*globals*/
char screen[50][50];
char contr[10]={'0','1','2','3','4','5','6','7','8','9'};
float xmin,xmax,ymin,ymax,xscale,yscale,xoff,yoff;

cxy(x,y,f,nx,ny,coln,zc,nc)
int nx,ny,nc,coln;float *x,*y,*zc,*f;
{/* contour f grid x(nx,ny),y(nx,ny)*/
float xp[4],yp[4],xd[3],yd[3],fd[3];
int i,j,k,np,nx1,ny1;
nx1=nx-1;ny1=ny-1;

DOFOR(i,nx1)
	{	
	DOFOR(j,ny1)
		{
/*k= j+i*ny ;printf(" i%d,j%d,k %d\n",i,j,k);*/
		xd[0]=x INDEX(i,j);
		xd[1]=x INDEX(i,j+1);
		xd[2]=x INDEX(i+1,j);
		yd[0]=y INDEX(i,j);
		yd[1]=y INDEX(i,j+1);
		yd[2]=y INDEX(i+1,j);
		fd[0]=f INDEX(i,j);
		fd[1]=f INDEX(i,j+1);		
		fd[2]=f INDEX(i+1,j);
/*printf(" xd %f %f %f yd %f %f %f fd %f %f %f\n",
xd[0],xd[1],xd[2],yd[0],yd[1],yd[2],fd[0],fd[1],fd[2]);
*/
		DOFOR(k,nc)
			{
			c3((xd),(yd),(fd),xp,yp,&np,zc[k]);
			p2(xp,(yp),np,(zc[k]),k);
			};
		xd[0]=x INDEX(i+1,j);
		xd[1]=x INDEX(i,j+1);
		xd[2]=x INDEX(i+1,j+1);
		yd[0]=y INDEX(i+1,j);
		yd[1]=y INDEX(i,j+1);
		yd[2]=y INDEX(i+1,j+1);
		fd[0]=f INDEX(i+1,j);
		fd[1]=f INDEX(i,j+1);
		fd[2]=f INDEX(i+1,j+1); 
/*printf(" xd %f %f %f yd %f %f %f fd %f %f %f\n",
xd[0],xd[1],xd[2],yd[0],yd[1],yd[2],fd[0],fd[1],fd[2]);
*/
		DOFOR(k,nc)
			{
			c3((xd),(yd),(fd),xp,yp,&np,zc[k]);
			p2(xp,(yp),np,(zc[k]),k);
			};
		}
	}
return;
}


p2(xp,yp,np,zc,k) float zc,*xp,*yp;int np,k;
{int i;
char tick;
if (np<0) return;
/*DOFOR(i,np)printf("p2 x=%f y=%f\n",xp[i],yp[i]);*/
/*if (zc==0.) tick='0';
else if (zc<0.) tick='-';
else 
	tick='+';	*/
tick=contr[k];

DOFOR(i,np)
	{
	drawf(xp[i],yp[i],xp[i+1],yp[i+1],tick);
	};
return;
}

drawf (x1,y1,x2,y2,symbol) char symbol; float x1,x2,y1,y2;
{int xfrom,xto,yfrom,yto;
xfrom= (x1-xoff)*xscale;
xto=(x2-xoff)*xscale;
yfrom=(y1-yoff)*yscale;
yto=(y2-yoff)*yscale;
/*printf(" x %d %d y %d %d\n",xfrom,xto,yfrom,yto);*/
draw(xfrom,yfrom,xto,yto,symbol);/* bresenham line plotter*/
return;
}

c3(xd,yd,fd,xp,yp,np,zc)
float xd[],yd[],fd[],xp[],yp[],zc; int *np;
{
int itl[3],itu[3],it,indexu,indexl;
float d,alpha,eps;
eps=1.e-28;
itl[0]=0;itl[1]=1;itl[2]=2;
itu[0]=1;itu[1]=2;itu[2]=0;
*np=-1;
/*printf(" xd %f %f %f yd %f %f %f fd %f %f %f\n",
xd[0],xd[1],xd[2],yd[0],yd[1],yd[2],fd[0],fd[1],fd[2]);
*/
DOFOR(it,3)
	{
	indexu=itu[it];
	indexl=itl[it];
	d= fd[indexu]-fd[indexl];
	if (abs(d) < eps) continue;
	alpha=(zc-fd[indexl])/d;
	if( alpha < 0. || alpha > 1.)continue;
	*np=(*np)+1;
	xp[*np]=xd[indexl]+alpha*(xd[indexu]-xd[indexl]);
	yp[*np]=yd[indexl]+alpha*(yd[indexu]-yd[indexl]);
/*printf(" np %d xp,yp %f %f %f %f %f\n",
*np,xp[*np],yp[*np],zc,fd[indexu],fd[indexl]);*/
	}
return;
}
setup()
{
xscale=(49.)/(xmax-xmin);
xoff=xmin;
yoff=ymin;
yscale=(49.)/(ymax-ymin);
cleara();
return;
}

main(argc,argv) int argc;char *argv[];
{float fmax,fmin;
int nx,ny,nc,k,i,j;
float x[50][50],y[50][50],r[50],z[50],f[50][50],zc[11];
int fin;
fin=fopen("plot.dat","r");
if(fin==-1)printf(" input pblm\n");
fscanf(fin,"%d %d",&nx,&ny);
printf(" nx,ny= %d %d\n",nx,ny);
fscanf(fin," %f ",&fmin);/* throw away first boundary point*/
DOFOR(i,nx-1) {fscanf(fin,"%f ",&(r[i]));/*printf(" r=%f\n",r[i]);*/}
fscanf(fin," %f ",&fmin);/* throw away first bounadry point*/
DOFOR(i,ny-1) {fscanf(fin,"%f ",&(z[i]));/*printf(" z=%f\n",z[i]);*/}

nc=5;
fmax=-1.e10;fmin=1.e10;
DOFOR(i,nx){
	DOFOR(j,ny)
		{
		x [i][j]=r[i];
		y [i][j]=z[j];
		fscanf(fin," %e ",&(f[i][j]));

			fmax= max(fmax,f[i][j]);
			fmin=min(fmin,f[i][j]);
		 };
	    };
nx--;ny--;
printf(" fmin,fmax= %f %f\n",fmin,fmax);

xmax=1.;
xmin=0.;
ymax=1.;
ymin=0.;
DOFOR(i,nc)zc[i]= ((float) (i+1) )*(fmax-fmin)/((float)(nc+1))+fmin;
DOFOR(i,nc){printf("contour %f symbol %c\n", zc[i],contr[i]);};
setup();
printf(" xoff %e xscale %e yoff %e yscale %e \n",xoff,xscale,yoff,yscale);
cxy(x,y,f,nx,ny,50,zc,nc);
show();	
exit(0);
}

