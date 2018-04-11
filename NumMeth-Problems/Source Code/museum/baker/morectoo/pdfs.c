/*
package of routines that compute the plasma dispersion
function for complex arguments, returning a complex value

allied functions computed are the error function and the Fresnel integrals

routines for handling complex arithmetic in C are included

from More C tools for scientists and engineers by L. Baker

*/

int iterp;/* global to return count used*/

/* for below, x,y are complex structures, and one is returned*/
#include "complex.h"
struct complex 	ci,c1,c0,o,o2,ir;

#define min(a,b) (((a)<(b))? (a): (b))
#define abs(x) ((x)?  (x):-(x))

main (argc,argv) int argc; char **argv;
{
/*FILE*/int *fopen(),*fileid;
int i;double erf(),erfc,a,z,x,y,si,ci,q,dawson(),ierfc();
fileid=fopen("PLOT.DAT","w");
fprintf(fileid," 4 \n");
q=1/.56418958;
printf(" ierfc %e %e\n",ierfc(.1,2),ierfc(.5,2));
printf(" ierfc %e %e\n",ierfc(.1,4),ierfc(.5,4));
printf(" ierfc %e %e\n",ierfc(.1,6),ierfc(.5,6));
printf(" scaled ierfc %e %e\n",q*ierfc(.1,1),q*ierfc(.5,1));
printf(" scaled ierfc %e %e\n",4.*ierfc(.1,2),4.*ierfc(.5,2));
printf(" scaled ierfc %e %e\n",32.*ierfc(.1,4),32.*ierfc(.5,4));
printf(" scaled ierfc %e %e\n",384.*ierfc(.1,6),384.*ierfc(.5,6));
printf(" scaled ierfc %e %e\n",122880.*ierfc(.1,10),122880.*ierfc(.5,10));
for (i=0;i<50;i++)
{
	x=(i+1)*.1;
	y=erf(x,&erfc);
	fresnel(x,&ci,&si);
	z=dawson(x);
printf(" x=%f %f %f %f %f %e %d\n",x,y,erfc,ci,si,z,iterp);
fprintf(fileid,"%f %e %e %e %e\n",x,y,erfc,ci,si);
};


exit(0);
}

printc(z) struct complex *z;
{
printf("%f %f",z->x,z->y);
}

double erf(z,erfc)
double *erfc,z;
{
struct complex cz1,cz,c4,czeta,czsp;
int iter;
double erff,exp();
c4.y=.5641895835;
c4.x=0.;
CMPLX(cz,0.,z);
iter=40;
pdisp(&cz,1.e-5,&czeta,&czsp,iter);
/*printf(" erf "); printc(&czeta);printc(&czsp);printf("\n");*/
CMULT(cz1,c4,czeta);
erff=1.+cz1.x*exp(-z*z);
*erfc=1.-erff;
return (erff);
}

double dawson(x) double x;
{
double eps=1.e-5;
int iter=30;
struct complex z,ans,derans;
z.x=x;z.y=0;
pdisp(&z,eps,&ans,&derans,iter);
return (-.5*ans.x);
}

/* compute the Fresnel integrals */

fresnel (z,fci,fsi) double z,*fci,*fsi;
{
int iter;
double rtip,aa;
struct complex ci,c1,c2,c3,cz,cc,czeta,czp,cdum,cdu1;
aa=.8862269255;
c1.x=aa;
c1.y=aa;
ci.x=0.;ci.y=1.;
c2.x=0.;
c2.y=1.570796327;
c3.x=-.5641895835;
c3.y=c3.x;
CTREAL(cz,c1,z);
iter=20;
pdisp(&cz,1.e-5,&czeta,&czp,iter);
/*printf(" fresnel "); printc(&czeta);printc(&czp);printf("\n");*/
CTREAL(cdum,c2, (z*z) );
cexp(&cdum,&cdu1);
CMULT(cdum,cdu1,czeta);
CMULT(cdu1,cdum,ci);
CTREAL(cdu1,cdu1,-.5);
cdu1.x=cdu1.x-aa;
CMULT(cdum,cdu1,c3);
*fci=cdum.x;
*fsi=cdum.y;
return;
}




/* compute the plasma dispersion function and its derivatives
iter is maximum iteration count allowed, eps desired error
zetai is the input argument, zeeo the value of the function
and zeeprimo the first derivative (both output)

*/
int itmax=20,lhpsw;
struct complex zeta,zee,zeeprim;
struct complex w,ww,g1,g2,ofo,z,zetasq,zp,bp,bpp1,bs,rz,iz;
struct complex crtpi,a1,a2,a3,b1,b2,b3,t1,t2,u1,u2,v1,v2,cdum,cdu2,cdu3,cdu1;
double pi=3.141592653589,sqrt(),rtpi,flhp,csw,xi1,ct1,ct2,cp1,app1,dreal;


pdisp(zetai,eps,zeeo,zeeprimo,iter) int iter;double eps;
struct complex *zetai,*zeeprimo,*zeeo;
{ci.x=0.;
ci.y=1.;
c1.x=1.;
c1.y=0.;
crtpi.x=0.;
crtpi.y=1.772453851;
c0.x=0.;c0.y=0.;
CASSN(zeta,zetai);
if(iter>0) itmax=iter;
if( zeta.x< 0.){lhpsw=-1;CONJG(cdum,zeta);CSUB(zeta,c0,cdum);}
	else {lhpsw=1;}
flhp=lhpsw;
if( zeta.y<0.) csw=-1.;
	else csw=1.;

CMULT(zetasq,zeta,zeta);
CMPLX(w, zeta.x, (csw*zeta.y));
CMULT(ww,w,w);
/*printf(" pdf w,ww ");printc(&w);printc(&ww);printf("\n");*/
if ( abs(zeta.y)>=1. || cabs(zeta)>10.) 
{
/* continued fraction approx for  abs( Im(zeta) ) >1 */
cfbig(eps);
}
else
{/* abs(zeta)<1*/
cfsmall(eps);
}
if(lhpsw==-1){
		CONJG(cdum,z);
		CSUB(z,c0,cdum);		
	     };
CLET(zee,z);
/*printf(" zee=");printc(&zee);printf("\n");*/
if(lhpsw==-1) {
		CONJG(cdum,zeta);
		CSUB(zeta,c0,cdum);
	      };
CMULT(zetasq,zeta,zeta);
if(cabs(zeta)>10.)
	{
	CLET(cdum,c1);cdum.x=cdum.x-csw;
	CMULT(cdu2,cdum,zetasq);
	CTREAL(cdu2,cdu2,-.5);
	cexp(&cdu2,&cdu3);
	CMULT(cdu1,cdum,cdu3);
	CMULT(zp,cdu1,crtpi);
	CLET(u1,c1);
	CLET(u2,c0);
	cdum.x=3.5;cdum.y=0.;
	CSUB(cdum,cdum,ww);
	cdu2.x=-1.5;cdu2.y=0.;
	CDIV(v1,cdu2,cdum);
	CLET(v2,c0);
	CLET(t1,v1);
	CLET(t2,c0);
	iterp=1;
	while(1==1)
		{
		app1= -(iterp+1)*(iterp+2.5);
		cdum.y=0;
		cdum.x=1.5+2*iterp;
		CSUB(bp,cdum,ww);
		CLET(bpp1,bp);bpp1.x=bpp1.x+2.;
		CMULT(bs,bpp1,bp);
		CTREAL(cdum,u1,app1);
		CADD(cdu2,cdum,bs);
		CDIV(u2,bs,cdu2);
		CSUB(cdum,u2,c1);
		CMULT(v2,v1,cdum);
		CADD(t2,v2,t1);
		CSUB(cdum,t2,t1);
		if(cabs(cdum)<eps || iterp>itmax) break;
		iterp++;
		CLET(u1,u2);
		CLET(t1,t2);
		CLET(v1,v2);
		}
		CMULT(cdum,zp,zeta);
		CTREAL(cdum,cdum,-2.);
		CSUB(cdu1,t2,ww);
		cdu2.x=cdu1.x+1.5;
		cdu2.y=cdu1.y *csw;
		CDIV(zeeprim,c1,cdu2);
		CTREAL(zeeprim,zeeprim,-1.);
		CADD(zeeprim,zeeprim,cdum);
if(lhpsw==-1)
		{
		CONJG(cdum,zeeprim);
		CLET(zeeprim,cdum);
		}
/*printf(" zeeprim=");printc(&zeeprim);printf("\n");*/
}
else
	{
	CMULT(cdum,zeta,z);
	CADD(cdum,c1,cdum);
	CTREAL(zeeprim,cdum,-2.);
	};	
CSET(zeeprimo,zeeprim);
CSET(zeeo,zee);
return;
}
cfbig(eps)double eps;
{
iterp=1;
CLET(a1,c1);
CLET(a2,c0);
cdum.x=2.5;cdum.y=0.;
CSUB(cdum,cdum,ww);
cdu2.x=-.5;cdu2.y=0.;
CDIV(b1,cdu2,cdum);
CLET(b2,c0);
CLET(t1,b1);
CLET(t2,c0);/*printf(" big\n");
printc(&a1);printc(&a2);printc(&b1);printc(&b2);printc(&t1);printc(&t2);*/
while(1)
	{
	app1=-(iterp+1)*(iterp+.5);
	dreal=.5+2*iterp;
	CLET(bp,c0); CSUB(bp,bp,ww);
	bp.x=bp.x+dreal;
	CLET(bpp1,bp);bpp1.x=bpp1.x+2.;
	CMULT(bs,bp,bpp1);		
	CTREAL(cdum,a1,app1);
	CADD(cdum,cdum,bs);
	CDIV(a2,bs,cdum);
	CSUB(cdum,a2,c1);
	CMULT(b2,b1,cdum);
	CADD(t2,t1,b2);
	CSUB(cdum,t2,t1);
/*printc(&a1);printc(&a2);printc(&b1);printc(&b2);printc(&t1);printc(&t2);
printc(&bp);printc(&bpp1);printc(&bs);printf(" %d\n",iterp);
*/
	if( cabs(cdum) < eps || iterp>itmax) break;
	iterp++;
	CLET(a1,a2);
	CLET(b1,b2);
	CLET(t1,t2);
	}	
CLET(cdum,c1);cdum.x=cdum.x-csw;
CTREAL(cdum,cdum,-.5);
CMULT(cdu2,cdum,zetasq);
cexp(&cdu2,&cdu3);
CLET(cdum,c1);cdum.x=cdum.x-csw;
CMULT(cdu2,cdum,cdu3);
CMULT(cdum,cdu2,crtpi);
CSUB(cdu3,t2,ww); 
CLET(cdu2,cdu3);
cdu3.x=cdu3.x+.5;
cdu1.x=cdu3.x;
cdu1.y= csw*cdu2.y;
CDIV(cdu3,zeta,cdu1);
CADD(z,cdum, cdu3);
}

cfsmall(eps)double eps;
{
xi1=1.;
CLET(b1,c1);
CLET(a1,c1);
ct1=2.5;
cp1=.5;
cdum.x=ww.x/ct1;cdum.y=ww.y/ct1;
CADD(b2,cdum,c1);
CTREAL(cdum,ww,.6666666666);
CSUB(a2,b2,cdum);
iterp=1;
/*printf(" small");*/
/* recursive calculation of kummer function*/
while(1==1){
	ct2=ct1*ct1;
	CTREAL(cdum,ww,(cp1/(ct2+ct1+ct1)));
	CADD(g1,c1,cdum);
	CTREAL(cdum,ww,(xi1*(xi1+cp1)/(ct2*(ct2-1.))));
     	CMULT(g2,cdum,ww);
	CMULT(cdum,g2,a1);
	CMULT(cdu2,g1,a2);
	CADD(a3,cdum,cdu2);
	CMULT(cdum,g2,b1);
	CMULT(cdu2,g1,b2);
	CADD(b3,cdum,cdu2);
	CDIV(cdu2,a2,b2);
	CDIV(cdu3,a3,b3);
	CSUB(cdum,cdu3,cdu2);
/*printc(&a1);printc(&a2);printc(&a3);printc(&b1);printc(&b2);printc(&b3);
printc(&g1);
printc(&g2);printf("%f %f %f %d\n",ct1,ct2,xi1,iterp);
*/
	if( cabs(cdum) <eps || iterp>itmax)break;
	CLET(a1,a2);
	CLET(b1,b2);
	CLET(a2,a3);
	CLET(b2,b3);
	ct1=ct1+2.;
	xi1=xi1+1.;
	iterp++;
     }
CMPLX(ofo,cdu3.x,(csw*cdu3.y));
CSUB(cdum,c0,zetasq);
cexp(&cdum,&cdu2);
CMULT(cdum,cdu2,crtpi);
CMULT(cdu2,zeta,ofo);
CTREAL(cdu2,cdu2,2.);
CSUB(z,cdum,cdu2);
/*printf(" ofo,z,iter"); printc(&ofo);printc(&z);printf(" %d\n",iterp);
*/
}

/* complex exponential function */

cexp( x,ans) struct complex *x,*ans;
{
double y,exp(),sin(),cos();
y = exp ( x->x);
ans->x= y*cos (x->y);
ans->y= y*sin (x->y);
return;
}

double ierfc(x,n) double x;int n;
{
double erf(),y,z,sqrt(),exp(),exch;
int k;
y=erf(x,&z);
if(!n)return z;
y=2./sqrt(pi)*exp(-x*x);
/* y=i^-1,z=i^0*/
for(k=1;1;k++)
	{
	y=(.5*y-x*z)/k;
	if(k==n)break;
	exch=z;
	z=y;
	y=exch;
	}
return y;
}
