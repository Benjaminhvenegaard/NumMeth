/* 2d multigrid simple test
From More C Tools for Scientists and Engineers by L. Baker*/
/* dimension to 2(n+1)**2 where n is a power of 2 */
float x[2000],v[2000],u[2000],f[2000];
/* x=grid v=answer f=drive  A for Laplacian, i.e Poisson Ay=f */
#include <stdio.h>
/*problem size, coarsest grid allowed*/
#define n 16 
#define ncoarsest 2

#define INDEX(I,J) [J+(I)*maxj]
#define INDX(I,J,k) [J+(I)*((k)+1)]
/*relaxtion parameters*/
#define n0 1
#define n1 2
#define n2 2
#define dx .1
float bc0=1.,bc1=2.,mult;/*boundary conditions*/

float av(i,j,m,y)
int i,j,m;  float *y;
{/* evaluate Av. use Av-f for residual*/
int maxj;
maxj=m+1;
/* boundary conditions residuals*/
if(!i||!j||i==(m)||j==(m))return y INDEX(i,j);
return (y INDEX(i,j+1)+y INDEX(i,j-1)+
   y INDEX(i+1,j)+y INDEX(i-1,j)-4.*y INDEX(i,j) )/dx*dx;
}

float residual(i,j,m,f,v)
int i,j,m;float *f,*v;
{
int maxj;
float av();
maxj=m+1;
return f INDEX(i,j)-av(i,j,m,v);
}

correct(m,v,v2h)
int m; float *v,*v2h;
{/*coarse to fine red points only */
int i,j,mfine,maxj;float save;
mfine=m<<1;
maxj=m+1;/*coarse*/
/*printf(" correcting coarse=%d\n",m);*/
for(j=0;j<=m;j++)
for(i=0;i<=m;i++)/*don't touch boundary points*/
	{/* i=0 set v 0,1 from v2h 0,1. i=m-1, set 2m-1*/
	if(i!=(m) && i && j && j!=(m) )
		{
		/* prevent correction of i==0 boundary here*/
		if(i)  /* agrees with comment, but not actual usage in MULTIG*/
		{
		save= v INDEX(2*i,2*j+1);
		/* multiply correction by 2 to account for no corr. of black*/
		v INDX(2*i,2*j+1,mfine)+=.5*mult*(v2h INDEX(i,j)+v2h INDEX(i,j-1));
/*printf("v %d %d was %f now %f\n",2*i,2*j+1,save,v INDX(2*i,2*j+1,mfine));*/
		}
/*add correction term*/
		save= v INDEX(2*i+1,2*j);
		v INDX(2*i+1,2*j,mfine)+=.5*mult*(v2h INDEX(i,j)+v2h INDEX(i-1,j));
/*printf("v %d %d was %f now %f\n",2*i+1,2*j,save,v INDX(2*i+1,2*j,mfine));*/
		}
/* if i==m but 2*i+1 would not be boundary- don't correct!
/*	else
		{;
		if(!i)
			{
			v INDX(0,2*j+1,k)+=.5*(v2h INDEX(i,j)+v2h INDEX(i,j-1));
			}
		else if(!j)
			{
			v INDX(2*i+1,j,k)+=.5*(v2h INDEX(i,j)+v2h INDEX(i-1,j));
			}
		else if (i==(m))
			{
			v INDX(2*i+1,2*j,k)+=.5*(v2h INDEX(i,j)+v2h INDEX(i,j-1));
			}
		else
			{
			v INDX(2*i,2*j+1,k)+=.5*(v2h INDEX(i,j)+v2h INDEX(i-1,j));
			}
		}
	*/
}
}

restrict(m,coarsef,finef,v)
int m; float *finef,*coarsef,*v;
{/*fine to coarse f*/
int i,mfine,j,k,l,finetop,maxj;
float coef,residp,residm,residc,residq,residr;
float residual();
maxj=m+1;/*coarse*/
mfine=m<<1;
finetop=mfine;
coef=dx*dx;
for(j=1;j<m;j++)
for(i=1;i<m;i++)
	{
	/* restrict  f2h = (fh-Av)*/
	k=i<<1;l=j<<1;
	residc= residual(k  ,l  ,mfine,finef,v);
	residm= residual(k-1,l  ,mfine,finef,v);
	residp= residual(k+1,l  ,mfine,finef,v);
	residq= residual(k  ,l+1,mfine,finef,v);
	residr= residual(k  ,l+1,mfine,finef,v);/*full weighting*/
	coarsef INDEX(i,j)=.125*(residp+residm+residq+residr+4.*residc);
	/*coarsef INDEX(i,j)=residc*/;/*injection*/
	}
	/* error residual  Ae=r;*/
for(j=0;j<m;j++)
	{
	coarsef INDEX(0,j)= finef INDX(0,j,mfine)-v INDX(0,j,mfine);
	coarsef INDEX(m,j)=finef INDX(finetop,j,mfine)-v INDX(finetop,j,mfine) ;
	}
for(i=0;i<m;i++)
	{
	coarsef INDEX(i,0)= finef INDX(i,0,mfine)-v INDX(i,0,mfine);
	coarsef INDEX(i,m)=finef INDX(i,finetop,mfine)-v INDX(i,finetop,mfine) ;
	}
}

relax(m,u,f)
int m;
float *u,*f;
{
int i,j,k,maxj;
float fip,fim,coef;
coef=dx*dx;
maxj=m+1;
/* solve approx. by sor iteration Au=f for u*/
/*boundary conditions Av=f a=1*/
for(i=0;i<=m;i++)
	{
	u INDEX(i,0)=f INDEX(i,0);
	u INDEX(i,m)=f INDEX(i,m);
	u INDEX(0,i)=f INDEX(0,i);
	u INDEX(m,i)=f INDEX(m,i);
	}
/*brute force*/
for(j=0;j<=m;j++)
for(i=0;i<=m;i++)
	{
/*	u INDEX(i,j)= ( u INDEX(i,j-1)+u INDEX(i,j+1)+u INDEX(i+1,j) 
	   +u INDEX(i-1,j)-f INDEX(i,j)*coef )*.25;
	   */
/*printf(" before: u %f f %f i,j %d %d\n",u INDEX(i,j), f INDEX(i,j) ,i,j);*/
	}

/* red-black*/

for(k=0;k<2;k++)
for(j=1+k;j<m;j+=2)
for(i=1;i<m;i++)
	{/* laplacian  u"= (u+ + u  -4*u)/dx^2=f */
	u INDEX(i,j)= ( u INDEX(i,j-1)+u INDEX(i,j+1)+u INDEX(i+1,j)
	   +u INDEX(i-1,j)-f INDEX(i,j)*coef )*.25;
	}
for(j=0;j<=m;j++)
for(i=0;i<=m;i++)
	{
/*	u INDEX(i,j)= ( u INDEX(i,j-1)+u INDEX(i,j+1)+u INDEX(i+1,j)
	   +u INDEX(i-1,j)-f INDEX(i,j)*coef )*.25;
	   */
/*printf(" after: u %f f %f i,j %d %d\n",u INDEX(i,j), f INDEX(i,j) ,i,j);*/
	}
}

mvcycle(nn,hin,v,f)
int hin,nn; float *v,*f;
{
/* perform MV scheme cycle  v initial guess returns v*/
int h,i,m,mu=1,msq;/* mu=1 for V scheme =2 for W scheme*/
float *nextgridf,*nextgridv;
h=hin;
m=nn;

/*printf(" enter mvcycle with h=%d n=%d\n",h,nn);*/
for(i=0;i<n1;i++)relax(nn,v,f);
if(ncoarsest!=nn)
	{
	msq=(m+1)*(m+1);
	nextgridf= &(f[msq]);
	nextgridv= &(v[msq]);
	m>>=1;
	msq=(m+1)*(m+1);
	h<<=1;
	restrict(m,nextgridf,f,v);
	for(i=0;i<msq;i++) *nextgridv++=0.;
	for(i=0;i<mu;i++)mvcycle(m,h,nextgridv ,nextgridf);
	correct(m,v,nextgridv);
	}
for(i=0;i<n2;i++)relax(nn,v,f);
}

fullmgv(nn,hin,v,f)
int nn,hin;float *v,*f;
{
int i,j,m,msq,h;
float *nextgridf,*nextgridv;
h=hin;
/*printf(" enter full with h=%d n=%d\n",h,nn);*/
if(nn!=ncoarsest)
	{
	m=nn>>1;
	h<<=1;
	msq=(nn+1)*(nn+1);
	nextgridf= &(f[msq]);
	nextgridv= &(v[msq]);
	restrict(m,nextgridf,f,v);
	msq=(m+1)*(m+1);
	for(i=0;i<msq;i++) *nextgridv++=0.;
	fullmgv(m,h,nextgridv ,nextgridf);
	correct(m,v,nextgridv);
	}
for(i=0;i<n0;i++)mvcycle(nn,hin,v,f);
}

FILE *plot;

main()
{
int i,j,k,l,m,twon,np,maxj,kount=0;
float resid,error,stopr;
float residual();
/*init*/
/* n, m, etc powers of two np 2**n+1. index runs 0-n, b.c. at 0,n */
np= n +1;maxj=np;
twon=2*np;
printf(" enter 1<=mult<=2,stop resid.");
scanf("%f %f",&mult,&stopr);
printf(" mult=%f stopping resid=%f\n",mult,stopr);
	/*initialize f,v all grids*/
	for(i=0;i< twon;i++)
	for(j=0;j<= n ;j++)
		{
		v INDEX(i,j)=0.;f INDEX(i,j)=0.;}
	/* b.c. for Av=f*/
l=n;
for(i=0;i<= n ;i++)
	{
	f INDEX(i,0)=bc0;
	f INDEX( n , i)=bc1;
	f INDEX( 0 ,i) =bc0;
	f INDEX(i, l ) =bc1;
	}
/* special test- v already ok*/
/*	for (i=0;i<n;i++)
		v[i]=bc0+(bc1-bc0)*i/((float)(n-1));
*/
while(1)
	{
/*	for (i=0;i<=n;i++)
	for (j=0;j<=n;j++)
		printf(" resid %e for v %e %d %d\n",
			residual(i,j,n,f,v),v INDEX(i,j),i,j);
*/
	fullmgv(n,1,v,f);
	/*calc. resid.*/
	resid=0.;
	for (j=0;j<=n;j++)
	for (i=0;i<=n;i++)
		{
		error = residual(i,j,n,f,v);
/*		printf(" error[%d][%d]=%f v=%f f=%f\n",
			i,j,error,v INDEX(i,j),f INDEX(i,j));
*/
		resid+=error*error;
		}
	kount++;
	if(resid < stopr ) break;
	else printf("RESIDUAL = %f IN MAIN at %d iteration\n",resid,kount);
        	}
printf(" fini\n");
plot=fopen("plot3.dat","w");
fprintf(plot," %d %d 0 1 0 1 X_axis Y_axis\n",np,np);
for(i=0;i<=n;i++){for(j=0;j<=n;j++)fprintf(plot,"%le ",v INDEX(i,j) );fprintf(plot,"\n");}
}

