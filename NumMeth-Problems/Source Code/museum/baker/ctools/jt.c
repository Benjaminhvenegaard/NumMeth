/*
Jenkins-Traub polynomial root finder

from Handbook of C tools for scientists and engineers by L. Baker

*/
/*
#include "libc.h"
#include "math.h"
*/
#include <stdio.h>

struct complex { double r;
		 double i;} ;
struct complex 	ci,c1,c0,o,o2,ir;
double d;/* dummy*/

int iterp;/* global to return count used*/

/* for below, x,y are complex structures, and one is returned*/


#define CADDR(x,y)  ( x.r+y.r)
#define CADDI(x,y)  (x.i+y.i)
#define CMULTR(x,y) (x.r*y.r-x.i*y.i)
#define CMULTI(x,y)  (x.i * y.r + x.r *y.i)
#define CDRN(x,y)  (x.r*y.r+y.i*x.i)
#define CDIN(x,y)  (x.i*y.r-x.r*y.i)
#define CNORM(x) (x.r*x.r+x.i*x.i)

#define CDIV(z,nu,de) {d=CNORM(de);z.r=CDRN(nu,de)/d;z.i=CDIN(nu,de)/d;}
#define CONJG(z,x) {z.r=x.r;z.i=-x.i;}
#define CONJ(x) {x.i=-x.i}
#define CMULT(z,x,y) {z.r=CMULTR((x),(y)); z.i=CMULTI((x),(y));}
#define CADD(z,x,y) {z.r=(x.r)+(y.r);z.i=(x.i)+(y.i);}
#define CSUB(z,x,y) {z.r=(x.r)-(y.r);z.i=(x.i)-(y.i);}
#define CLET(to,from) {to.r=from.r;to.i=from.i;}
#define cabs(x) sqrt(x.i*x.i+x.r*x.r)
#define CMPLX(x,real,imag) {x.r=(real);x.i=(imag);}
#define CASSN(to,from) {to.r=from->r;to.i=from->i;}
#define CASS(to,from) {to->r=from->r;to->i=from->i;}
#define CSET(to,from) { to->r=from.r;to->i=from.i;};
#define CTREAL(z,x,real) {z.r=x.r*(real);z.i=x.i*(real);}

#define abs(x) ((x>0.)? (x) : -(x) )
#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)>=(b))? (a): (b))

double baker;

main (argc,argv) int argc; char **argv;
{
FILE *fopen(),*fileid;
int degree,i,fail;
double x,y,cmod();
float fudge;
struct complex a,b,z,coef[4],ans[3];
/* test problem*/
CMPLX(a,3.,4.);x= cmod(&a);
printf(" test of cmod=%e\n",x);
CMPLX(a,1.,0.); CMPLX(b,0.,1.);
cdivid(&a,&b,&z);
printf(" enter fudge factor for eta");
scanf("%f",&fudge);
baker=fudge;
printf(" eta scaled by %e\n",baker);
printf(" test cdivid=");printc(&z);printf("\n");
degree=3;
CMPLX(coef[0],1.,0.);
CMPLX(coef[1],0.,0.);
CMPLX(coef[2],-1.,0.);
CMPLX(coef[3],-1.,0.);
jt(coef,degree, ans,&fail);
if(fail)
	{
	printf(" failed\n");
	}
for(i=0;i<3;i++)
	{
	printf(" root=");
	printc(&(ans[i]));
        printf("\n");
	}
exit(0);
}

printc(z) struct complex *z;
{
printf("%f %f",z->r,z->i);
}


#define MAXDEG 50
struct complex p[MAXDEG],h[MAXDEG],qp[MAXDEG],qh[MAXDEG],sh[MAXDEG];
struct complex s,t,pv;
double are,mre,eta,infin,smalno,base,tempa[MAXDEG],tempb[MAXDEG];
double xx,yy,cosr,sinr;
int nn;

jt(op,degree,zero,fail)
int *fail,degree;
struct complex op[],zero[];
{
double bnd,xxx,scale(),cmod(),sqrt(),cauchy();
int i,idnn2,cntl1,cntl2,nm1,conv;
struct complex z;
mcon(&eta,&infin,&smalno,&base);
are=eta;/* factor of 2 my addition*/
mre=2.*sqrt(2.)*eta;
xx=.70710678;
yy=-xx;
cosr=-.069756474;
sinr=.99756405;
*fail=0;
nn=degree+1;

CMPLX(t,-99.,-99.);/* is t initially undefined in fxshft?*/
if (op[0].r==0. && op[0].i==0.)
	{
	*fail=1;
	return;
	}
for(; (op[nn-1].r==0. && op[nn-1].i==0.)&&nn>=0 ;nn--)
	{
	printf(" zero constant term found\n");
	idnn2=degree-nn+1;
	CMPLX(zero[idnn2],0.,0.);
	}
for(i=0;i<nn;i++)
	{
	CLET(p[i],op[i]);
	tempa[i]=cmod(&(p[i]));	
	}
bnd=scale(nn,tempa,&eta,&infin,&smalno,&base);
if(bnd!=1.)
	{
	for(i=0;i<nn;i++)
		{
		CTREAL( (p[i]),(p[i]),bnd);
		}
	}

findzero:
if(nn==1)return;
if(nn<=2)
	{
	cdivid(&p[1],&p[0],&zero[degree-1]);
	CTREAL( zero[degree-1],zero[degree-1],-1.);
	return;
	}
for(i=0;i<nn;i++)
	{
	tempa[i]=cmod(&(p[i]));
	}
bnd=cauchy(nn,tempa,tempb);
for(cntl1=0;cntl1<=1;cntl1++)
	{
	noshft(5);
	for(cntl2=1;cntl2<=9;cntl2++)
		{
		xxx=cosr*xx-sinr*yy;
		yy=sinr*xx+cosr*yy;
		xx=xxx;
		s.r=bnd*xx;
		s.i=bnd*yy;
		fxshft(10*cntl2,&z,&conv);
		if(conv)
			{
			idnn2=degree-nn+1;
			CLET(zero[idnn2],z);
			nn--;
			for(i=0;i<nn;i++)
				{CLET(p[i],qp[i]);
				}
			goto findzero;
			}
		}
	}
*fail=1;
return;
}


double errev(nn,q,ms,mp,are,mre)
int nn;
struct complex *q;
double ms,mp,are,mre;
{
double ans,e,cmod();
int i;
e=cmod(&(q[0])) * mre/(are+mre);
for(i=0;i<nn;i++)
	{
	e=e*ms+cmod(&(q[i]));
	}
ans=e*(are+mre)-mp*mre;
return(ans);
}

nexth(boolean)
int *boolean;
{
double t1,t2;
int n,nm1,j;
n=nn-1;
/*nm1=n-1;*/
if(*boolean)
	{
	for(j=1;j<n;j++)
		{
		CLET(h[j],qh[j-1]);
		}
	CMPLX(h[0],0.,0.);
	return;
	}	
/*else*/
for(j=1;j<n;j++)
	{
	CMULT(h[j],t,qh[j-1])
	CADD(h[j],h[j],qp[j])
	}
CLET(h[0],qp[0]);
return;
}

double cauchy(nn,pt,q)
double pt[],q[]; int nn;
{
int n,i,nm;
double x,dx,df,log(),exp(),f,xm;
pt[nn-1]*=-1.;
n=nn-1;
nm=n-1;
x= exp(log(-pt[n]))-log(pt[0])/((double)n);
if(pt[nm]!=0.)
	{xm=-pt[n]/pt[nm];
	x=min(xm,x);
	}
repeat:
xm=x*.1;
f=pt[0];
for(i=1;i<nn;i++)
	f=f*xm+pt[i];
if(f>0.)
	{
	x=xm;
	goto repeat;
	}
dx=x;
while( abs(dx/x)> .005)
	{
	q[0]=pt[0];
	for (i=1;i<nn;i++)
		q[i]=q[i-1]*x+pt[i];
	f=q[n];
	df=q[0];
	for (i=1;i<n;i++)
		df=df*x+q[i];
	dx=f/df;
		x-=dx;
        }
return(x);
}

calct(boolean)
int *boolean;
{
int n,nm1,i;
double cmod();
struct complex hv;
n=nn-1;   nm1=n-1;
polyev(n,&s,h,qh,&hv);
*boolean=  (cmod(&hv)<= are*10.*cmod(&(h[nm1])) );
if(*boolean)
	{
	CMPLX(t,0.,0.);	
	return;
	}
cdivid(&pv,&hv,&t);
CTREAL(t,t,-1.);
return;
}
vrshft(l3,z,conv)
int *conv,l3;
struct complex *z;
{
double cmod(),mp,ms,omp,relstp,r1,r2,sqrt(),errev(),tp;
int boolean,b,j,i;
b=0;
*conv=0;
CASSN(s,z);
for(i=0;i<l3;i++)
	{
	polyev(nn,&s,p,qp,&pv);
	mp=cmod(&pv);
	ms=cmod(&s);
	if(mp<=(20.*errev(nn,qp,ms,mp,are,mre)) )
		{
		*conv=1;
		CSET(z,s);
		return;
		}	
	if(i!=0)
		{
		if( !b && !(mp<omp) && (relstp<.05))
				{
				tp=relstp;
				b=1;
				if(relstp<eta)tp=eta;
				r1=sqrt(tp);
				r2=s.r*(1.+r1)-s.i*r1;
				s.i=s.r*r1+s.i*(1.+r1);
				s.r=r2;
				polyev(nn,&s,p,qp,&pv);
				for(j=0;j<5;j++)
						{
						calct(&boolean);
						nexth(&boolean);	
						}
				omp=infin;
				goto skp;
				}
		if(mp*.1 >omp)return;
		}
	omp=mp;
skp:
	calct(&boolean);
	nexth(&boolean);
	calct(&boolean);
	if(!boolean)
		{
		relstp=cmod(&t)/cmod(&s);
		CADD(s,s,t);
		}
	}
return;
}

fxshft(l2,z,conv)
int l2,*conv;
struct complex *z;
{
int i,j,n,test,pasd,boolean;
double cmod();
struct complex svs,ot,lou;
n=nn-1;
polyev(nn,&s,p,qp,&pv);
test=1;
pasd=0;
calct(&boolean);
for(j=0;j<l2;j++)
	{
	CLET(ot,t);
	nexth(&boolean);
	calct(&boolean);
	CADD((*z),s,t);
	if( (!boolean)&&test && (j!=(l2-1))  )
			{
			CSUB(lou,t,ot);
			if( cmod(&lou)>= .5*cmod(z) )
					{pasd=0;}
			else if (!pasd) {pasd=1;}
			else
				{
				for(i=0;i<n;i++)
						{
						CLET(sh[i],h[i]);
						}
				CLET(svs,s);
				vrshft(10,z,conv);
				if(*conv)
					{
					return;
					}
				test=0;
				for(i=0;i<n;i++)
						{
						CLET(h[i],sh[i]);
						}
				CLET(s,svs);
				polyev(nn,&s,p,qp,&pv);
				calct(&boolean);
				}
			}
		else
			{
			}
	}
vrshft(10,z,conv);
return;
}

noshft(l1)
int l1;
{
int n,nm1,i,j,jj;
double xni,cmod(),t1,t2;
n=nn-1;
nm1=n-1;
for(i=0;i<n;i++)
	{
	xni=((double)(n-i))/((double) n);
	CTREAL(h[i],p[i],xni);
	}
for(jj=0;jj<l1;jj++)
	{
	if(cmod(&(h[nm1]))> eta*10.*cmod(&(p[nm1])))
		{
		cdivid(&(p[n]),&(h[nm1]),&t);
		CTREAL(t,t,-1.);
		for(i=1;i<=nm1;i++)
			{
			j=nn-i-1;
		/*	t1=h[j-1].r;
			t2=h[j-1].i;
			h[j].r=t.r*t1-t.i*t2;
			h[j].i=t.r*t2+t.i*t1;*/
			CMULT(h[j],t,h[j-1]);
			CADD(h[j],h[j],p[j]);
			}
		CLET(h[0],p[0]);
		}
	else
		{
		for(i=1;i<=nm1;i++)
			{
			j=nn-i-1;
			CLET(h[j],h[j-1]);
			}
		CMPLX(h[0],0.,0.);
		}
	}
return;
}

polyev(nn,s,p,q,pv)
int nn;
struct complex *s,p[],q[],*pv;
{/* nested polynomial evaluation*/
int i;
double t; struct complex temp;
CLET(q[0],p[0]);
CLET((*pv),q[0]);
for(i=1;i<nn;i++)
	{
/*	t=pv->r *s->r -pv->i*s->i +p[i].r;
	pv->i=pv->r*s->i+pv->i*s->r+p[i].i;
	pv->r=t;
*/
	CMULT(temp,(*pv),(*s));
	CADD((*pv),temp,(p[i]));
	CASSN(q[i],pv);
	}
return;
}


mcon(eta,infin,smalno,base)
double *eta,*infin,*smalno,*base;
{
/* will compute eta*/
printf(" entered mcon");
for(*eta=1.; (1.+(*eta))>1.;(*eta)*=.5);
*eta *= baker;/* my modification*/
	printf(" eta=%e\n",*eta);
/*
for(*smalno=1.e-30;*smalno*.5>0;*smalno*=.5)
*/
*smalno=(1.e-30);
		printf(" smalno=%e\n",*smalno);
/* set by hand as some compilers can't handle overflows/underflows*/
*infin=(1.e30);
*base=2.;

printf(" leaving mcon\n");
return;
}

double cmod(x) struct complex *x;
{/* robust but expensive version of cabs()*/
double ans,sqrt(),rpart,ipart,aux;
rpart= x->r; ipart= x->i;
rpart= abs(rpart);
ipart= abs(ipart);
if(rpart>ipart)
	{
	aux= ipart/rpart;
	ans=rpart*sqrt(1.+ aux*aux);
	return(ans);
	}
else if (rpart<ipart)
	{
	aux=rpart/ipart;
	ans=ipart*sqrt(1.+aux*aux);
	return(ans);
	}
/*else*/
return (rpart*sqrt(2.));
}

cdivid(a,b,c) struct complex *a,*b,*c;
{/* robust c=a/b*/
double r,d,dummy,infin;
if( b->r==0. && b->i==0.)
	{
	mcon(&dummy,&infin,&dummy,&dummy);
	c->r=infin;
	c->i=infin;
	return;
	}
if (abs(b->r)>=abs(b->i))
	{
	r=b->i/b->r;
	d=1./(b->r + r*b->i);
	c->r=(a->i *r + a->r)*d;
	c->i=(-a->r *r +a->i)*d;
	}
else
	{
	r=b->r/b->i;
	d=1./(b->i + r*b->r);
	c->r=(a->r *r + a->i)*d;
	c->i=(a->i *r - a->r)*d;
	}
return;
}

double scale(nn,pt,eta,infin,smalno,base)
double pt[],*eta,*infin,*smalno,*base;
int nn;
{
int i;
double scal,maxx,minn,hi,lo,x,sc,sqrt(),log(),pow();
hi=sqrt(*infin);
lo=*smalno/(*eta);
printf(" hi %e low %e nn %d\n",hi,lo,nn);
maxx=0.;
minn=*infin;
for(i=0;i<nn;i++)
	{
	x=pt[i];
	maxx= max(x,maxx);
	if(x!=0.)minn=min(x,minn);
	}
scal=1.;
if(minn>=lo && maxx<=hi)
	{
	return(scal);
	}
x=lo/minn;
if(x>1.)
	{
	sc=x;
	if( *infin/sc >maxx) sc=1.;
	}
else
	{
	sc=1./(sqrt(maxx)*sqrt(minn));
	}
printf(" before log,pow %e\n",sc);
i= (int) (log(sc)/log(*base)+.5);
scal= pow(*base, ((double) i));
return(scal);
}

