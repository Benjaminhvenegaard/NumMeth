/*
Singular Value Decomposition (SVD) of a matrix
from Handbook of C Tools for Scientists and Engineers by L. Baker

CONTENTS:
	svd()	calculates SVD
	diag()	second half of SVD calculation
	dsign() type double function. equivalent to FORTRAN sign 
		transfer function
	type double versions of vector.c and matrix.c routines:
	printmd	

DEPENDENCIES:
	NONE	
*/
/*
  #include "libc.h"
  #include "math.h"
 */
/* defines below are from ftoc.h except for IU,DFFOR */
#define INDEX(i,j) [j+(i)*coln]
#define IU(i,j) [j+(i)*colu]
#define DOFOR(i,j) for(i=0;i<j;i++)
#define DFFOR(i,from,to) for(i=from;i<to;i++)
#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)<(b))? (b): (a))
#define abs(x)  ( ((x)>0.)?(x):-(x))


#define itmax 30


printmd(a,coln,rown,col,row) int rown,row,col,coln; double a[];
{
int i,j,btm,top,count;
printf("\n");
btm=top=0;
while(btm<col)
	{
	top=min(col,(btm+8));
	printf(" printing matrix columns %d to %d\n",btm,(top-1));
	DOFOR(j,row)
		{
		for(i=btm;i<top;i++)
			{
			printf(" %e",a INDEX(j,i));
			}
		printf("\n");
		}
	btm+=8;
	}
return;
}
svd(coln,m,n,a,w,matu,u,matv,v,ierr,rv1)
int coln,m,n,matu,matv,*ierr;
double a[],w[],u[],v[],rv1[];
/*
	all matrices a,u,v have coln>=n columns

	a(m,n)  v(n,n) u(m,n or m )

typically, m>n.

matu = 0 do not compute u
       1 compute  u(m,n)
      -1 compute u(m,m)
NB: U MUST BE DIMENSIONED (M,N) IF MATU =1 AND (M,M) IF MATU=-1
matv = 0 do not compute v
       1 compute v
ierr= error return integer 
rv1	working storage (see text)
*/
{
int i,j,k,mn,i1,l,l1,its,m1,n1,k1,top,colu;
double sqrt(),dsign(),dum,c,f,g,h,s,x,y,z,eps,scale,anorm;
*ierr=0;
colu=n;
if (matu<0)colu=m;
/*printmd(a,coln,m,n,m);*/

m1=m-1;
n1=n-1;
DOFOR(i,m)
	{
	DOFOR(j,n) u IU(i,j)= a INDEX(i,j);
	}
/* reduction to bidiagonal form via Householder xfms*/
g=0.;
scale=0.;
anorm=0.;
DOFOR(i,n)
	{
	l=i+1;
	rv1[i]=g*scale;
	g=0.;
	s=0.;
	scale=0.;
	if(i<=m1)/* in fortran, <= but c i runs 0-m1*/
		{
		DFFOR(k,i,m)scale+= abs(u IU(k,i));	
		if(scale!=0.)
			{
			DFFOR(k,i,m)
				{
				dum=u IU(k,i)/scale;
				s+=dum*dum;	
				u IU(k,i)=dum;
				}
			f= u IU(i,i);
			g= -dsign(sqrt(s),f);
			h=1./(f*g-s);/* 1/h of others- * faster than / */
			u IU(i,i)=f-g;
			if (i!=n1)
				{
				DFFOR(j,l,n)
					{
					s=0.;
					DFFOR(k,i,m)s+=
					 u IU(k,i)*u IU(k,j);
					f=s*h;
					DFFOR(k,i,m)
						u IU(k,j)+=f*u IU(k,i);
					}
				}/*i!=n*/
			DFFOR(k,i,m)u IU(k,i)*=scale;
			}/*scale!=0*/
		}/* i<=m*/
w[i]=scale*g;
g=0.;
s=0.;
scale=0.;
if(i<=m1 && i!=n1)
	{
	DFFOR(k,l,n)scale+= abs(u IU(i,k));
	if(scale!=0.)
		{
		DFFOR(k,l,n)
			{
			dum= u IU(i,k)/scale;
			s+=dum*dum;
			u IU(i,k)=dum;
			}
		f=u IU(i,l);
		g=-dsign(sqrt(s),f);
		h=1./(f*g-s);
		u IU(i,l)=f-g;
		DFFOR(k,l,n)rv1[k]=u IU(i,k)*h;
		if(i!=m1)
			{
			DFFOR(j,l,m)
				{
				s=0.;
				DFFOR(k,l,n)
					s+=u IU(j,k)*u IU(i,k);
				DFFOR(k,l,n)u IU(j,k)+=s*rv1[k];

				}

			}
		DFFOR(k,l,n)u IU(i,k)*=scale;
		}
	}
y=abs(w[i])+abs(rv1[i]) ;
anorm= max( anorm , y);
	}
/*accum. rhs*/
/*printf(" acc rhs\n");*/
if(matv)
	{
	for(i=n-1;i>=0;i--)
		{
		if(i!=n1)
			{
			if (g!=0.)
				{/*here FMM do division by g,u indiv*/
				/*h=1./(g*u IU(i,l));*/
				DFFOR(j,l,n)v INDEX(j,i)
					=(u IU(i,j)/u IU(i,l) )/g;
				DFFOR(j,l,n)
					{s=0.;
					DFFOR(k,l,n)s+=
						u IU(i,k)*v INDEX(k,j);
					DFFOR(k,l,n)v INDEX(k,j)+=
						s* v INDEX(k,i);
					}
				}
			DFFOR(j,l,n)
				{
				v INDEX(i,j)=0.;
				v INDEX(j,i)=0.;
				}
			}
		v INDEX(i,i)=1.;
		g=rv1[i];
		l=i;
		}
	}
/*accum. lhs*/
/*printf(" accum lhs\n");*/
if(matu)
	{
	if(matu==-1)
		{
		DFFOR(i,n,m)
			{
			DFFOR(j,n,m) u IU(i,j)=0.;
			u IU(i,i)=1.;
			}
		}	
	mn=n;
	if(m<n) mn=m;
	top=n;
	if(matu==-1)top=m;
	for(i=mn-1;i>=0;i--)
		{
		l=i+1;
		g=w[i];
		if (i!=n1)
			{
			DFFOR(j,l,top)u IU(i,j)=0.;
			}
		if(g!=0.)
			{
			/*h= 1./(g* u IU(i,i));*/
			if(i!=(mn-1))
			{
			DFFOR(j,l,top)
				{
				s=0.;
				DFFOR(k,l,m)
					s += u IU(k,i) * u IU(k,j);
				f=(s/u IU(i,i))/g;
				DFFOR(k,i,m)u IU(k,j)+=f*u IU(k,i);
				}/* DFFOR j */
			}/* i!=mn-1*/
				DFFOR(j,i,m)u IU(j,i)/=g;
			
			}
		else/*g=0*/
			{
			DFFOR(j,i,m)u IU(j,i)=0.;
			}		
		u IU(i,i)+=1.;
		}
	}
diagon(u,v,rv1,w,anorm,coln,colu,n,m,ierr);
return;
}
diagon(u,v,rv1,w,anorm,coln,colu,n,m,ierr)
double anorm,u[],v[],rv1[],w[];
int coln,colu,n,m,*ierr;
{
int i,j,k,l,l1,nm1,k1,i1,matu,matv,its;
double c,s,h,f,g,x,z,y;
matu=1;matv=1;
/*
printf(" diagonalizing,anorm=%f\n",anorm);
pvd(w,n);pvd(rv1,n);
printmd(u,coln,m,n,m);
printmd(v,coln,n,n,n);
*/
/* diagonalize binary form*/
for(k=n-1;k>=0;k--)
	{
	k1=k-1;
	its=0;
testsplit:
	for(l=k;l>=0;l--)
		{
		l1=l-1;
	/*	if ( abs(rv1[l])<=eps)goto conv;
		if(abs(w[l1])<=eps)break;
	*/
		if ( abs(rv1[l])+anorm==anorm)goto conv;
		if(abs(w[l1])+anorm==anorm)break;
		}
	c=0.;
	s=1.;
	DFFOR(i,l,k+1) /* want i to range from l to k inclusive */
		{
		f=s*rv1[i];
		rv1[i]*=c;	
/*	if(abs(f)>eps)
*/
	if(abs(f)+anorm!= anorm )
			{
			g=w[i];
			h=sqrt(f*f+g*g);
			w[i]=h;
			c=g/h;
			s=-f/h;
			if(matu)
				{
				DOFOR(j,m)
					{
					y=u IU(j,l1) ;
					z=u IU(j,i) ;
					u IU(j,l1)=y*c+z*s;
					u IU(j,i)=-y*s+z*c;
					}
				}/*matu*/
			}/* if */
		}/* DFFOR i,l,k*/
		conv:;
		z=w[k];
		if(l!=k)
			{
			if (its>=itmax)
				{
				*ierr=k;
				return;
				}
			its++;
			x=w[l];
			y=w[k1];
			g=rv1[k1];
			h=rv1[k];
			f=((y-z)*(y+z)+(g-h)*(g+h))/(2.*h*y);
			g=sqrt(f*f+1.);
			f=((x-z)*(x+z) +h*(y/(f+dsign(g,f))-h))/x;
			/* do another QR xfm*/
			c=1.;
			s=1.;
			DFFOR(i1,l,k1+1)/* i1 ranges l to k1 inclusive*/
				{
				i=i1+1;
				g=rv1[i];
				y=w[i];
				h=s*g;
				g=c*g;
				z=sqrt(f*f+h*h);
				rv1[i1]=z;
				c=f/z;
				s=h/z;
				f=x*c+g*s;
				g=-x*s+g*c;
				h=y*s;
				y=y*c;
				if(matv)
					{
					DOFOR(j,n)
						{
						x= v INDEX(j,i1);
						z= v INDEX(j,i);
						v INDEX(j,i1)=
						   x*c+z*s;
						v INDEX(j,i)=
						   -x*s+z*c;
						}
					}
				z=sqrt(h*h+f*f);
				w[i1]=z;
				if(z!=0.)
					{
					c=f/z;
					s=h/z;
					}
				f=c*g+s*y;
				x=-s*g+c*y;
				if(matu)
					{
					DOFOR(j,m)
						{
						y= u IU(j,i1);
						z= u IU(j,i);
						u IU(j,i1)=
						   y*c+z*s;
						u IU(j,i)=
						   -y*s+z*c;
						}
					}	
				}
			rv1[l]=0.;
			rv1[k]=f;
			w[k]=x;
			goto testsplit;	
			}/*l!=k*/		
	/*convergence*/
	if(z<0.)
		{/* make w[k] non-neg*/
		w[k]=-z;
		if(matv)
		{
		DOFOR(j,n) v INDEX(j,k)=-v INDEX(j,k);
		}
		}	
	}
return;
}

double dsign(to,from) double to,from;
{
double x;
x=0.;
if(from>0.)x=1.;
if(from<0.)x=-1.;
return( x*abs(to));
}
