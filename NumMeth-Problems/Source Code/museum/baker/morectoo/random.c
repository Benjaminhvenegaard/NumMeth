/*
package of routines to generate and apply random numbers

from More C Tools for Scientists and Engineers by L. Baker

*/
/* You may change stmt below, replacing random() with u16() or u32() */

#define urand()  random()

long int seed;
int naflag,nflag;
long int s1,s2;
int s16,s26,s36;
double u32(),u16(),random();
double ex(),ca(),na();

double erlang(k,mean) int k; double mean;
{
double exp(),expon(),emult,sum;
int i;
emult= mean/k;
sum=0.;
for (i=0;i<k;i++) sum+=exp(expon(emult));
return(sum);
}

double expon(mean) double mean;
{/* assumes rand has been seeded with proper iy*/
double log(),urand();
return(-mean*log(urand()) );
}

double cauchy()
{
double tan(),pi=3.141592653589793,random();
return  tan((pi-random())*.5);
}

double logistic(a,k) double a,k;
{
/* logistic distribution F= 1/{1+exp-(x-a)/k }
	mean a, variance b^2= (k*pi)^2/3  */
double random(),log(),x;
x=random();
return a+k*log(x/(1.-x));
}

int randi(nvalue,probd) int nvalue;float probd[];
/* random integer betwen 1-nvalue in accord with probd distrib.*/
{
int i,n1;
double u;
u=rand();/* u(0,1) uniform random */
n1=nvalue-1;
for (i=0;i<n1;i++) if(u<probd[i])return(i);
return(nvalue);
}

double uniform(a,b) double a,b;
{
double urand();
return(a+(b-a)*urand());
}

double normal(mean,sd,s1,s2) double mean,sd,*s1,*s2;
{
double log(),urand(),sqrt(),r1,r2,v1,v2,s,ss;
int iter;
iter=0;
while(1)
	{
	r1=urand();r2=urand();
	v1=2.*r1-1.;v2=2.*r2-1.;
	s=v1*v1+v2*v2;
	if (s<1.)
		{
		ss=sqrt(-2.*log(s)/s)*sd;
		*s1=v1*ss+mean;
		*s2=v2*ss+mean;
		return(*s1);
		};
/*	iter++;
	if(iter>10)
		{
		printf(" trouble in normal %f %f %f\n",r1,r2,s);
		break;
		}
*/
	};/*end while*/
	return(0.);/* Keep DeSmet happy*/
}

double snorm1,snorm2;

double norm(mean,sd) double mean,sd;
{
/* nflag must be initialized to 1 before first call*/
nflag^=1;
if(nflag)return snorm2;
return normal(mean,sd,&snorm1,&snorm2);
}
/* TEST CODE BELOW
*/
main(argc,argv) int argc; char **argv;
{
int i,j,k;
double urand(),normal(),mean,sd,z,y,x,xs,ys,rk,meant,m2,var,mu3,mu4,skew,kurt;
double sum1,sum2,sum3,sum4;
double u,random();
long int outseed;
seed=1;
naflag=1;
nflag=1;
s1=12345;s2=67890;
s16=12;s26=23;s36=34;
for(i=0;i<=10000;i++)
	{outseed=seed;
	u=random();
	}
printf(" u=%f seed = %ld \n",u,outseed);
printf(" results of u16:\n");
for(i=0;i<=1000;i++)
	{u=u16();
	if(i%100 ==0)printf(" u=%f \n",u);
	}
printf(" results of u32:\n");
for(i=0;i<=1000;i++)
	{u=u32();
	if(i%100 ==0)printf(" u=%f \n",u);
	}
/* now test distributions */
mean=0.;sd=1.;
while(1)
{printf(" enter count");scanf("%d",&k);
if(k<=0)break;
printf(" kount=%d\n",k);
sum1=sum2=sum3=sum4=0.;
for (i=0;i<k;i++)
	{
	u=normal(mean,sd,&x,&y);
/*	printf(" return normal %f %f\n",x,y);*/
	sum1=sum1+x+y;
	xs=x*x;ys=y*y;
	sum2=sum2+xs+ys;
	sum3=sum3+xs*x+ys*y;
	sum4=sum4+xs*xs+ys*ys;
	}
printf(" tabulating\n");
rk=.5/k;
meant=sum1*rk;
m2=meant*meant;
var=sum2*rk-m2;
mu3=(sum3-3.*sum1*sum2*rk)*rk+2.*meant*m2;
mu4=sum4*rk-3.*m2*m2+6.*m2*sum2*rk-4.*sum3*sum1*rk*rk;
skew=mu3/(var*sqrt(var));
kurt=(mu4/(var*var)-3.)*.5;
/* mean should equal 0. approximately.
   var    "      "   1.     "         (variance=sd*sd)
  skewness           0.
kurtosis             0.                as defined here, which is
CRC Basic Stat. tables definition.  Others define kurtosis without
the fact of .5 and/or without the -3.
*/

printf(" mean=%f,var=%f,skew=%f,kurt=%f\n",meant,var,skew,kurt);
}/* end while*/
mean=0.;sd=1.;
printf(" now fast normal dist mean=0 var=1\n");
while(1)
{printf(" enter count");scanf("%d",&k);
if(k<=0)break;
printf(" kount=%d\n",k);
sum1=sum2=sum3=sum4=0.;
for (i=0;i<k;i++)
	{
	x=na();y=na();
	sum1=sum1+x+y;
	xs=x*x;ys=y*y;
	sum2=sum2+xs+ys;
	sum3=sum3+xs*x+ys*y;
	sum4=sum4+xs*xs+ys*ys;
	}
printf(" tabulating\n");
rk=.5/k;
meant=sum1*rk;
m2=meant*meant;
var=sum2*rk-m2;
mu3=(sum3-3.*sum1*sum2*rk)*rk+2.*meant*m2;
mu4=sum4*rk-3.*m2*m2+6.*m2*sum2*rk-4.*sum3*sum1*rk*rk;
skew=mu3/(var*sqrt(var));
kurt=(mu4/(var*var)-3.)*.5;
/* mean should equal 0. approximately.
   var    "      "   1.     "         (variance=sd*sd)
  skewness           0.
kurtosis             0.                as defined here, which is
CRC Basic Stat. tables definition.  Others define kurtosis without
the fact of .5 and/or without the -3.
*/
printf(" mean=%f,var=%f,skew=%f,kurt=%f\n",meant,var,skew,kurt);
}/* end while*/
printf(" now exponential dist\n");
mean=0.;sd=1.;
while(1)
{printf(" enter count");scanf("%d",&k);
if(k<=0)break;
printf(" kount=%d\n",k);
sum1=sum2=sum3=sum4=0.;
for (i=0;i<k;i++)
	{
	x=ex();y=ex();
/*	printf(" return normal %f %f\n",x,y);*/
	sum1=sum1+x+y;
	xs=x*x;ys=y*y;
	sum2=sum2+xs+ys;
	sum3=sum3+xs*x+ys*y;
	sum4=sum4+xs*xs+ys*ys;
	}
printf(" tabulating\n");
rk=.5/k;
meant=sum1*rk;
m2=meant*meant;
var=sum2*rk-m2;
mu3=(sum3-3.*sum1*sum2*rk)*rk+2.*meant*m2;
mu4=sum4*rk-3.*m2*m2+6.*m2*sum2*rk-4.*sum3*sum1*rk*rk;
skew=mu3/(var*sqrt(var));
kurt=(mu4/(var*var));
/* mean should equal 0. approximately.
   var    "      "   1.     "         (variance=sd*sd)
  skewness           0.
kurtosis             0.                as defined here, which is
CRC Basic Stat. tables definition.  Others define kurtosis without
the fact of .5 and/or without the -3.
*/

printf(" mean=%f,var=%f,skew=%f,kurt=%f\n",meant,var,skew,kurt);
}/* end while*/
mean=0.;sd=1.;
printf(" now cauchy\n");
while(1)
{printf(" enter count");scanf("%d",&k);
if(k<=0)break;
printf(" kount=%d\n",k);
sum1=sum2=sum3=sum4=0.;
for (i=0;i<k;i++)
	{
	x=ca();y=ca();
/*	printf(" return normal %f %f\n",x,y);*/
	sum1=sum1+x+y;
	xs=x*x;ys=y*y;
	sum2=sum2+xs+ys;
	sum3=sum3+xs*x+ys*y;
	sum4=sum4+xs*xs+ys*ys;
	}
printf(" tabulating\n");
rk=.5/k;
meant=sum1*rk;
m2=meant*meant;
var=sum2*rk-m2;
mu3=(sum3-3.*sum1*sum2*rk)*rk+2.*meant*m2;
mu4=sum4*rk-3.*m2*m2+6.*m2*sum2*rk-4.*sum3*sum1*rk*rk;
skew=mu3/(var*sqrt(var));
kurt=(mu4/(var*var));
/* mean should equal 0. approximately.
   var    "      "   1.     "         (variance=sd*sd)
  skewness           0.
kurtosis             0.                as defined here, which is
CRC Basic Stat. tables definition.  Others define kurtosis without
the fact of .5 and/or without the -3.
*/

printf(" mean=%f,var=%f,skew=%f,kurt=%f\n",meant,var,skew,kurt);
}/* end while*/

exit(0);
}


double u32()
{/*32 bit*/
long int z,k;
k= s1/ 53668;
s1= 40014*(s1-k*53668)-k*12211;
if(s1<0) s1+=2147483563;
k= s2/52774;
s2=40692*(s2-k*52774)-k*3791;
if(s2<0)s2+=2147483399;
z=s1-s2;
if(z<1)z+=2147483562;
return z*4.656613e-10;
}


double u16()
{/*16 bit*/
int z,k;
k= s16/ 206;
s16= 157*(s16-k*206)-k*21;
if(s16<0) s16+=32363;
k= s26/217;
s26=146*(s26-k*217)-k*45;
if(s26<0)s26+=31727;
k= s36/222;
s36=142*(s36-k*222)-k*133;
if(s36<0)s36+=31657;
z=s16-s26;
if(z>706)z-=32362;
z+=s36;
if(z<1)z+=32362;
return z*3.0899e-5;
}

double random()
{
long int a=16807,m=2147483647,q=127773,r=2836;
long int lo,hi,test;
hi= seed / q;
lo= seed % q;
test= a*lo-r*hi;
seed=(test>0)?test: test+m;
return (double) seed/m;
}

double ex()
{
static double ln2=.6931471805599453,a=5.7133631526454228,
b=3.4142135623730950,c=-1.6734053240284925,p=.9802581434685472,
q=5.6005707569738080,r=3.3468106480569850,h=.0026106723602095,
d=.08576764376269050;
double g,aux,u,up,y,random(),exp(),aux2,aux1,H=2.28421e-4;
/*H=h*d/p;*/
u=random();
g=c;
dbl: u=u+u;
if(u<1.)
	{
	g+=ln2;
	goto dbl;
	}
u--;
if(u<p) return (g+q/(r-u));
while(1)
	{
	u=random();
	aux=b-u;
	y= a/aux;
	up=random();
	aux1=(up*H+d)*aux*aux;      aux2=exp(-(y+c));
	if( aux1 <= aux2 )return (g+y);
	}
return(0.);/*for DeSmet*/
}

double ca()
{
static double a=.6380631366077803,b=.5959486060529070,
q=.93399629257603656,w=.2488703380083841,c=.6366197723675813,
d=.5972997593539963,h=.0214949094570452,p=4.9125013953033204;
double u,random(),t,s,up,x;
u=random();
t=u-.5;
s=w-t*t;
if(s>0.0)return t*(c/s+d);
while(1)
	{
	u=random();
	t=u-.5;
	s=.25-t*t;
	up=random();
	x=t*(a/s+b);
	if( s*s*((1+x*x)*(h*up+p)-q)+s<=.5)return x;
	}
return(0.);
}
static double y;

double na()
{
int b;
double ex(),random(),t,up,u,e,s,ca,x,r;
static double a=.6380631366077803,g=.5959486060529070,
q=.93399629257603656,w=.2488703380083841,c=.6366197723675813,
d=.5972997593539963,h=.0214949094570452,p=4.9125013953033204;
/* initialize naflag=1*/
naflag ^= 1;
if(naflag) return y;
u=random();
b=(u<.5)? 0:1;
e=ex();
r=e+e;
/* ca= cauchy dist*/
u= (b)? u+u-1. : u+u;
t=u-.5;
s=w-t*t;
if(s>0.0) ca=t*(c/s+d);
else while(1)
	{
	u=random();
	t=u-.5;
	s=.25-t*t;
	up=random();
	ca=t*(a/s+g);
	if( s*s*((1+ca*ca)*(h*up+p)-q)+s<=.5)break;
	}
x=sqrt(r/(1.+ca*ca));
y=ca*x;
if(!b) return x;
else return -x;
return 0.;
}

