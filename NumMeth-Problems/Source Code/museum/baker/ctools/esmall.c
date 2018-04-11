/*
find smallest eigenvalues of a symmetric square matrix

from Handbook of C Tools for Scientists and Engineers by L. Baker

uses Rayleigh quotient iteration (easily changed to inverse iteration)

int esmall(a,value,vector,work,worka,pivot,workp,n,m,eigvect)
	matrix a(n,n). find the m eigenvalues of smallest(see text)
	absolute value. If eigvect nonzero, return eigenvalues
	as well in vector[].  Eigenvalues returned in value.
	REturns count of eigenvalues found.  Other arguments work space.

DEPENDENCIES:
	ftoc.h
	vector.c routines
	linear algebra routines in luf.c,lus.c,luback.c
*/

#include <ftoc.h>
/*#include "libc.h"
#include "math.h"
*/

#define wait 6
#define waitrs 3
#define maxit 100
#define tola 1.e-9
#define tolr .00000
#define tola2 .000000
#define tolr2 .000000

int ess(a,value,vector,work,worka,pivot,workp,n,m,eigvect)
int n,m,pivot[],workp[],eigvect;
float a[],value[],vector[],work[],worka[];
/* find m smallest eigenvalues of symmetric matrix a(n,n) 
 by inverse iteration  Rayleigh quotient iteration*/
/* pivot,work must be at least n long*/
/* the ith eigenvalue is returned in value[i-1]
	"       vector is in the i-th ROW of vectors
note that the rows may have been swapped. This info is in pivot.
eigvect=0 if we only want eigenvalues

*/
{
double sqrt();
double enew,eold;
double alpha,resid,deltrq,rnew,dot(),sqnor(),rq,rqold,shift,shifto;
int iflag,info,i,j,it,ke,nn,nn1,coln,index,k;
float *v1,sdot();
nn=n;
coln=n;
index=0;
rqold=1.e10;
rnew=resid=1.e6;
DOFOR(i,m*n) vector[i]=0.;
DOFOR(ke,m)
	{
	nn1=nn-1;
	shift=0.;/* will it still work midway between two eigenvalues?YES*/
	if(ke>0)shift=value[ke-1];
		/*above is attempt to insure the smallest m eigenvalues
			are found */
	shifto=shift;
	v1=&(vector[coln*ke]);
	if(nn==1)
		{value[ke]= a[0];
		v1[0]=1.00;
/* set single known component of eigenvector to 1. (arb.)
 this will work except in the rare case its identically zero-
*/		
		iflag=1;
		pivot[ke]=0;
printf(" going to loner\n");
		goto loner;
		}
	vset(work,1.,n);work[0]=-.5;
	DOFOR(it,maxit)
		{
		vcopy(work,v1,nn);
		/*shift*/
/*printm(a,coln,nn,nn,nn);*/
		mcopy(a,worka,coln,nn,nn);
		DOFOR(index,nn) worka INDEX(index,index)-= (shift);
/*printm(worka,coln,nn,nn,nn);*/
		lufact(worka,coln,nn,workp,&info);
/*printf(" info=%d\n",info);pv(v1,nn);*/
		if (info!=0)
				{/* we are there v1 is eigenvector*/
				rq= 0.;/*eigenvalue=shift */
				goto convg;
				}
		backsub(worka,coln,nn,workp,v1);
/*pv(v1,nn);*/
		/* Rayleigh quotient*/
		rq= dot(v1,work,nn)/dot(v1,v1,nn);
/*printf(" shift=%f,rq=%f\n",shift,rq);*/
		alpha=1./ sqrt(sqnor(v1,nn));
		vs(v1,alpha,nn);/*renormalize v1*/
		alpha=1./ sqrt(sqnor(work,nn));
		vs(work,alpha,nn);
		if(it>wait)
			{/* check for termination*/
/*pv(v1,nn);pv(work,nn);*/
			if( v1[0]*work[0]<0.)
				vs(work,-1.,nn);
			/* don't need work any more*/
			vdif(v1,work,work,nn);
			rnew= sqnor(work,nn);
			deltrq=abs(rq-rqold);
/*printf("dif=");pv(work,nn);
printf(" rnew=%f,resid=%f\n",rnew,resid);*/
			if ( rnew*(alpha*alpha)<tolr ||rnew<tola
			    || deltrq<tola2
			    || deltrq<tolr2*rq)
				{/*converged*/
				convg:
				value[ke]=shift+rq;
/*printf(" eigenvalue=%f ke=%d\n",value[ke],ke);	
printf(" eigenvector:");pv(v1,nn);*/
				/*deflate*/
/*printf(" before deflation");printm(a,coln,coln,nn,nn);*/
				alpha=0.;/*find max element*/
				DOFOR(i,nn)
					{resid=abs(v1[i]);
					 if(resid>alpha)
						{
						j=i;
						alpha=resid;
						}
					}
				pivot[ke]=j;
			/*swap eigenv elements, a row and col j<->nn*/
				index=nn-1;
				vcopy(v1,work,nn);
				if(j!=index)
					{
					resid=work[j];
					work[j]=work[index];
					work[index]=resid;
					swaprow(a,index,j,coln);
					swapcol(a,index,j,coln);
					alpha=1./resid;
					}
				else
					alpha=1./work[j];
				
				work[index]=1.;
				DOFOR(i,index) work[i]*=alpha;
				/* work[nn-1]=1., all other work<1*/
/*printf(" work vector=\n");	pv(work,nn);*/
				DOFOR(i,index)
					{
printf(" deflating row %d\n",i);
					DOFOR(j,index)
						{
						a INDEX(i,j)-=
						work[i]* a INDEX(index,j);
						/* stmt ok DeSmet, error Aztec*/
printf("work=%f,a=%f,i=%d j=%d index=%d\n",work[i],a INDEX(index,j),i,j,index);
						
						}
					}	
					nn--;
printf(" after deflation ");printm(a,coln,coln,coln,coln);
					iflag=0;
loner:				if( eigvect && ke>0)
					{
					enew=value[ke];
					index=ke;
					for(j=coln-ke;j<coln;j++)
					{
					index--;
					eold=value[index];
					if(enew==eold)
							{
							printf(" degenerate eigenvalues\n");
							goto giveup;
							}
					alpha= dot(&(a INDEX(j,0) ),v1,j)/(enew-eold);
printf(" eold %f enew %f j %d ke %d\n",eold,enew,j,ke);
printf(" alpha=%f\n",alpha);
					v1[j]=alpha;
					/* multiply by all previous t matrices, latest first 
					 this is the permuted vector,not just vector!
					 don't unpermute until done */
pv(v1,j+1);
					/* modify alpha to account for fact eigenvectors
					are not stored with last component (biggest)=1.
					we do not use this last component of the prev eigvector
					in vv() below. 
					*/
					vcopy(&(vector[coln*(index)]),work,n);
					i=pivot[index];
					if(i!=0)
						{
						resid=work[i];
						work[i]=work[j];
						work[j]=resid;
						}
					alpha/=work[j];
printf(" norm alpha by %f,new alpha%f\n",vector[coln*index+j],alpha);	
					vv(v1,v1,work,alpha,j);
pv(v1,j+1);
pv(&(vector[coln*index]),j);
					normv(v1,j+1);
printf(" renorm v now\n");pv(v1,j+1);
					
					}

					if(iflag)goto fini;
	
					}/*if */
				giveup: break;
				}/* if converged*/
/*			if(rnew>resid*1.1)
				{
printf(" diverging %f %f eigenvalues permuted\n",rnew,resid);
				return(ke);
				}
*/
			}/* it> wait*/
		/*update iteration*/
		resid=rnew;
		rqold=rq;
		vcopy(v1,work,nn);
		shifto=shift;
		if(it>waitrs)shift+=rq;/*rayleigh quotient iteration*/
		}/* do it*/
	if(it>=(maxit-1)){/* iteration count exceeded,no conv*/
			printf(" no converg, eigevalues permuted\n");
			 return(ke);
			}
	if(nn==0)break;
	}/* loop over eignvalues*/
fini:
DOFOR(i,m)printf(" pivot[%d]=%d\n",i,pivot[i]);

for(i=1;i<m;i++)
	{/* which element to pivot*/
	/*permute eigenvalues*/
	k=pivot[i-1];
	if(k)
		{
		for(j=i;j<m;j++)/*loop over eigenvectors to be affected*/ 
			{ 
			index= j*coln+k;
			ke= j*coln+ (coln-i) ;/*coln-i was nn for ith eigenv.*/
			alpha= vector[index];
			vector[index]=vector[ke];
			vector[ke]=alpha;
			}
		}	
	}
/*graham Schmidt improvement orthog. with respect to previous eigenv.
    cannot do sooner easily due to permutations*/
if(eigvect)
{
DOFOR(i,m)
	{
	DOFOR(j,i)
		{
		alpha= dot(&(vector[i*coln]),&(vector[j*coln]),n);
		DOFOR(k,n) vector[i*coln+k]-=alpha*vector[j*coln+k];		
		}
	normv(&(vector[i*coln]),n);
	}
}
return(m);
}
