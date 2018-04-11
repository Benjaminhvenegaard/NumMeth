/* 1d multigrid simple test
from More C Tools for Scientists and Engineers by L. Baker*/
/* dimension to two times n */
float x[100],v[100],u[100],f[100];
/* x=grid v=answer f=drive  A for Laplacian, i.e Poisson Ay=f */

/*problem size, coarsest grid allowed*/
#define n 32
#define ncoarsest 4

/*relaxtion parameters*/
#define n0 1
#define n1 2
#define n2 2
#define dx .1
float bc0=1.,bc1=2.;/*boundary conditions*/

float av(i,m,y)
int i,m;  float *y;
{/* evaluate Av. use Av-f for residual*/
if(!i) return y[0];
if(i==(m-1))return y[m-1];
return (y[i+1]+y[i-1]-2.*y[i])/(dx*dx);
}

float residual(i,m,f,v)
int i,m;float *f,*v;
{
float av();
return f[i]-av(i,m,v);
}

correct(m,v,v2h)
int m; float *v,*v2h;
{/*coarse to fine*/
int i,mfine;
mfine=m<<1;
for(i=0;i<m;i++)/*don't touch boundary points*/
	{/* i=0 set v 0,1 from v2h 0,1. i=m-1, set 2m-1*/
	if(i!=(m-1))
		{
		v[2*i]+=v2h[i];/*add correction term*/
		v[2*i+1]+=.5*(v2h[i]+v2h[i+1]);
		}
	else 
		{
		v[2*i]+=.5*(v2h[i]+v2h[i-1]);
		v[2*i+1]+=v2h[i];
		}
	}
}

restrict(m,coarsef,finef,v)
int m; float *finef,*coarsef,*v;
{/*fine to coarse f*/
int i,mfine,j,finetop;
float coef,residp,residm,residc;
float residual();
mfine=m<<1;
finetop=mfine-1;
coef=dx*dx;
for(i=1;i<m;i++)
	{
	/* restrict  f2h = (fh-Av)*/
	j=i<<1;
	residc= residual(j  ,mfine,finef,v);
	residm= residual(j-1,mfine,finef,v);
	residp= residual(j+1,mfine,finef,v);
	coarsef[i]=.25*(residp+residm+2.*residc);/*full weighting*/
	/*coarsef[i]=residc*/;/*injection*/
	}
	/* error residual  Ae=r;*/
	coarsef[0]= finef[0]-v[0];
	coarsef[m-1]=finef[finetop]-v[finetop];
	
}

relax(m,u,f)
int m;
float *u,*f;
{
int i,k;
float fip,fim,coef;
coef=dx*dx;
/* solve approx. by sor iteration Au=f for u*/
u[0]=f[0];u[m-1]=f[m-1];/*boundary conditions Av=f a=1*/
for(i=1;i<m-1;i++)
	{/* laplacian  u"= (u+ + u-  -2*u)/dx^2=f */
	u[i]= ( u[i+1]+u[i-1] -f[i]*coef )*.5;
	}
}

mvcycle(nn,hin,v,f)
int hin,nn; float *v,*f;
{
/* perform MV scheme cycle  v initial guess returns v*/
int h,i,m,mu=1;/* mu=1 for V scheme =2 for W scheme*/
float *nextgridf,*nextgridv;
h=hin;
m=nn;
/*printf(" enter mvcycle with h=%d n=%d\n",h,nn);*/
for(i=0;i<n1;i++)relax(nn,v,f);
if(ncoarsest!=nn)
	{
	nextgridf= &(f[m]);
	nextgridv= &(v[m]);
	m>>=1;
	h<<=1;
	restrict(m,nextgridf,f,v);
	for(i=0;i<m;i++) *nextgridv++=0.;
	for(i=0;i<mu;i++)mvcycle(m,h,nextgridv ,nextgridf);
	correct(m,v,nextgridv);
	}
for(i=0;i<n2;i++)relax(nn,v,f);
}



fullmgv(nn,hin,v,f)
int nn,hin;float *v,*f;
{
int i,j,m,h;
float *nextgridf,*nextgridv;
h=hin;
/*printf(" enter full with h=%d n=%d\n",h,nn);*/
if(nn!=ncoarsest)
	{
	m=nn>>1;
	h<<=1;
	nextgridf= &(f[nn]);
	nextgridv= &(v[nn]);
	restrict(m,nextgridf,f,v);
	for(i=0;i<m;i++) *nextgridv++=0.;
	fullmgv(m,h,nextgridv ,nextgridf);
	correct(m,v,nextgridv);
	}
for(i=0;i<n0;i++)mvcycle(nn,hin,v,f);
}

main()
{
int i,j,k,l,m,twon;
float resid,error;
float residual();
/*init*/
twon=2*n;
for(i=0;i<n;i++)
	{x[i]= i*dx;
	 v[i]=0.;/*initial guess*/
	 f[i]=0.;
	 }
/*boundary conditions */
/*iterate*/
	/*initialize f,v all grids*/
	for(i=0;i<twon;i++)
		{v[i]=0;f[i]=0.;}
	/* b.c. for Av=f*/	
	k=n;j=0;
	f[j]=bc0;f[k+j-1]=bc1;
/* special test- v already ok*/
/*	for (i=0;i<n;i++)
		v[i]=bc0+(bc1-bc0)*i/((float)(n-1));
*/
while(1)
	{
/*	for (i=0;i<n;i++)
		printf(" resid %e for v %e\n",residual(i,n,f,v),v[i]);
*/
	fullmgv(n,1,v,f);
	/*calc. resid.*/
	resid=0.;
	for (i=0;i<n;i++) 
		{
		error = residual(i,n,f,v);
		printf(" error[%d]=%f v=%f\n",i,error,v[i]);
		resid+=error*error; 
		}
	if(resid < .01) break;
	else printf("RESIDUAL = %f IN MAIN\n",resid);
	}
printf(" fini\n");
}

