/*
matrix eigenvalues/eigenvectors via QR decomposition
from Handbook of C Tools for Scientists and Engineers by L. Baker

CONTENTS:

eigenvv() computes eigenvalues and eigenvectors of a matrix
hqr2()	  applies QR method to Hessenberg matrix
elmhes()	converts general matrix to Hessenberg form
eltran()	creates transform. matrix from elmhes output
balance()	balances a matrix
balbak()	inverse transform of balance(used on eigenvectors)
pnormwr()	normalize and print eigenvectors. largest element=1.

type double versions of vector.c and matrix.c routines:
printmd

DEPENDENCIES:
NONE
*/
/*
  #include "libc.h"
*/
#include <math.h>

/* defines below are from ftoc.h except for IU,DFFOR,DFFR,DFFRR */
#define INDEX(i,j) [j+(i)*coln]
#define IU(i,j) [j+(i)*colu]
#define DOFOR(i,j) for(i=0;i<j;i++)
#define DFFOR(i,from,to) for(i=from;i<to;i++)
#define DFFR(i,from,to) for(i=from;i<=to;i++)
#define DFFRR(i,from,to) for(i=from;i>=to;i--)
#define min(a,b) (((a)<(b))? (a): (b))
#define max(a,b) (((a)<(b))? (b): (a))
/*#define abs(x)  ( ((x)>0.)?(x):-(x)) using fabs */


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

double macheps;

eigenvv(coln,n,a,iwork,scale,z,wr,wi,bal,cnt)
int coln,n,iwork[],bal,cnt[];
double *a,*scale,*z,wr[],wi[];
{
int i,low,high,m,ierr;
if (bal)
	{
	balance(coln,n,a,&low,&high,scale);
	printmd(a,coln,n,n,n);
/*	DOFOR(i,n)printf(" scale=%e\n",scale[i]);
	printf (" balance low=%d high=%d\n",low,high);*/
	}
else
	{
	low=0;
	high=n-1;
	}	
elmhes(coln,n,low,high,a,iwork);
printmd(a,coln,n,n,n);
printf(" low=%d high=%d\n",low,high);   
/*DOFOR(i,n)printf(" intar=%d\n",iwork[i]);*/
eltran(coln,n,low,high,a,iwork,z);
printmd(z,coln,n,n,n);
   hqr2(coln,n,low,high,a,wr,wi,z,&ierr,cnt);
   if(ierr) printf(" ierr=%d\n",ierr);
   m=n;/*unless fewer returned?*/
/*hqr2 uses z to transform eigenvectors which are returned in z */
if(ierr)return;
if(bal)
	{ 
	balbak(coln,n,low,high,scale,m,z);
	}
}

hqr2(coln,n,low,igh,h,wr,wi,vecs,ierr,cnt)
int coln,n,low,igh,*ierr,cnt[];
double *vecs,*h,wr[],wi[];
{
/* coln, n have "fortran" values
low,igh  have    "fortran-1" values */

   int i,j,k,l,m,ii,jj,kk,mm,na,nm,nn,its, mpr,enm2,en;
   int it1=10,it2=20,itlim=30,notlast;
   double p,q,r,s,t,w,x,y,z,ra,sa,vi,vr,zz,norm;
   double xr,yr,xi,yi,zr,zi,sqrt();
   *ierr=0;
/* balance must have been called first! */
/* calculation of norm here as in EISPAK fortran code*/
   norm=0.;
   k=0;

   DOFOR(i,n)
   {
      if(i<low || i>igh)
      {
         wr[i]= h INDEX(i,i);
         wi[i]=0.0;
      }
      for(j=k;j<n;j++) norm+=fabs(h INDEX(i,j));
      k=i;
      cnt[i]=0;
   }
   en=igh;
   t=0.;
nextw:
   if(en>=low)
   {
      its=0;
      na=en-1;
      enm2=na-1;
nextit:
      DFFRR(l,en,low)/*algol code had one less iteration than ftn*/
      {/* as in EISPAC*/
      s=fabs(h INDEX(l-1,l-1) )+ fabs(h INDEX(l,l) );
      if(s==0.)s=norm;
         if( fabs(h INDEX(l,l-1) ) <= s*macheps )goto break1;
      }
      l=low;
      break1:
      x= h INDEX(en,en);
      if(l==en)
      {/*found a root*/
/*printf(" single root its=%d\n",its);*/
         wr[en]= x+t;
         wi[en]=0.;
         h INDEX(en,en)=x+t;
         en=na;
         cnt[en]=its;
         goto nextw;
      }
      y= h INDEX(na,na);
      w= h INDEX(en,na) * h INDEX(na,en);
      if(l!=na)
      {
         if(its==itlim)
         {
         	cnt[en]=31;
            *ierr=en;
            return;
         }
         if(its==it1 || its==it2)
         {
            t+=x;
            DFFR(i,low,en) h INDEX(i,i) -=x;
            s= fabs( h INDEX(en,na) ) +fabs( h INDEX(na,en-2));
            x=.75*s;
            y=x;
            w=-0.4375*s*s;
         }
         its++;
/*printf(" its=%d\n",its);*/
         for(m=en-2;m>=l;m--)
         {
            mm=m+1;
            z= h INDEX(m,m);
            r=x-z;
            s=y-z;
            p=(r*s-w)/(h INDEX(mm,m)) + h INDEX(m,mm);
            q= h INDEX(mm,mm)-z-r-s;
            r= h INDEX(m+2,mm);
            s= fabs(p)+fabs(q)+fabs(r);
            p/=s;
            q/=s;
            r/=s;
            if(m==l)break;
            if( fabs( h INDEX(m,m-1)) * (fabs(q)+fabs(r)) <=
              macheps*fabs(p)*
              (fabs(h INDEX(m-1,m-1))+fabs(z)+fabs(h INDEX(mm,mm)))
              )break;
         }

         for(i=m+2;i<=en;i++) h INDEX(i,i-2)=0.;
         for(i=m+3;i<=en;i++) h INDEX(i,i-3)=0.;
         for(k=m;k<=na;k++)
         {
            notlast= (k!=na);
            if(k!=m)
            {
               p= h INDEX(k,k-1);
               q= h INDEX(k+1,k-1);
               r= notlast ? h INDEX(k+2,k-1) : 0.;
               x=fabs(r)+fabs(q)+fabs(p);
               if (x==0.)break;/*exit for loop*/
               p/=x;q/=x;r/=x;
            }
            s= sqrt(p*p+q*q+r*r);
            if (p<0.) s=-s;
            if (k!=m) 
               h INDEX(k,k-1)=-s*x;
            else 
               if(l!=m)
               h INDEX(k,k-1)=-h INDEX(k,k-1);
            p+=s;
            x=p/s;y=q/s;z=r/s;q/=p;r/=p;
            /*row modification*/
            for(j=k;j<n;j++)
            {
               p= h INDEX(k,j)+q*h INDEX(k+1,j);
               if (notlast)
               {
                  p+=r*h INDEX(k+2,j);
                  h INDEX(k+2,j) -=p*z;
               }
               h INDEX(k+1,j) -=p*y;
               h INDEX(k,j) -=p*x; 
            }
            j= (k+3)<en ? k+3 : en ;
            /*column mods*/
            DFFR(i,0,j)
            {
               p=x*h INDEX(i,k)+y*h INDEX(i,k+1);
               if (notlast)
               {
                  p+=z*h INDEX(i,k+2);
                  h INDEX(i,k+2) -=p*r;
               }
               h INDEX(i,k+1) -=p*q;
               h INDEX(i,k)-=p;
            }
            /*accumulate*/
            for(i=low;i<=igh;i++)
            {
               p=x* vecs INDEX(i,k)+y*vecs INDEX(i,k+1);
               if(notlast)
               {
                  p+=z*vecs INDEX(i,k+2);
                  vecs INDEX(i,k+2)-=p*r;
               }
               vecs INDEX(i,k+1) -=p*q;
               vecs INDEX(i,k) -=p;
            }
         }/*end of for k loop*/
        goto nextit;
      }/* l!=na block*/
/* else, l=na, two roots found*/
/*printf(" double its=%d\n",its);*/
      p=.5*(y-x);
      q=p*p+w;
      z=sqrt(fabs(q));
      x+= t;
      h INDEX(en,en)=x;
      h INDEX(na,na)=y+t;
	 cnt[en]=-its;cnt[na]=its;
      if (q>=0.) /*q>0 in algol */
      {/*real pair*/
	     z= p<0. ? p-z : p+z ;
         wr[na]=x+z;
		 wr[en]=wr[na];  /*EISPACK version */
         if (z !=0.)
         		{
		         s=x-w/z;         		
         		wr[en]=s;
         		}
         wi[en]=0.;
         wi[na]=0.;
         x=h INDEX(en,na);
         /* r=sqrt(x*x+z*z); 
         p=x/r;
         q=z/r;
         algol version */
         s= fabs(x)+fabs(z);
         p=x/s;
         q=z/s;
	 r= sqrt(p*p+q*q);
         p/=r;
         q/=r;
         for(j=na;j<n;j++)
         {
            z=h INDEX(na,j);
            h INDEX(na,j) = q*z+p*h INDEX(en,j);
            h INDEX(en,j) = q*h INDEX(en,j)-p*z;
         }
         DFFR(i,0,en)
         {
            z=h INDEX(i,na);
            h INDEX(i,na)=q*z+p*h INDEX(i,en);
            h INDEX(i,en)=q*h INDEX(i,en)-p*z;
         }
         for(i=low;i<=igh;i++)
         {
            z=vecs INDEX(i,na);
            vecs INDEX(i,na)=q*z+p*vecs INDEX(i,en);
            vecs INDEX(i,en)=q*vecs INDEX(i,en)-p*z;
         }
      } /* end if real pair */
      else /*complex pair*/
      {
         wr[en]=x+p;
         wr[na]=x+p;
         wi[na]=z;
         wi[en]=-z;
      }
      en--;en--;
      goto nextw;
   }

/* all eigenvalues found*/

   for(en=n-1;en>=0;en--)
   {
      p=wr[en];
      q=wi[en];
      na=en-1;
      if(q==0.)
      {
         /*real vector*/
         m=en;
         h INDEX(en,en)=1.;
         for(i=na;i>=0;i--)
         {
            w=h INDEX(i,i)-p;
            r=h INDEX(i,en);
            DFFR(j,m,na)
            {
               r+=h INDEX(i,j)*h INDEX(j,en);
            }
            if(wi[i]<0.0)
            {
               z=w;
               s=r;
            }
            else
            {
               m=i;
               if (wi[i]==0.)
               {
                  h INDEX(i,en)= -r/( (w!=0.)? w : macheps*norm);
               }
               else
               {
                  x=h INDEX(i,i+1);
                  y=h INDEX(i+1,i);
                  q=(wr[i]-p);
                  yr=wi[i];
                  q=q*q+ yr*yr;
                  t=(x*s-z*r)/q;
                  h INDEX(i,en)=t;
                  h INDEX(i+1,en)= (fabs(x)>fabs(z))?
                    (-r-w*t)/x :(-s-y*t)/z ;
               }/*wi!=0*/
            } /*wi>=0*/
         }/*for i loop*/
      }/*real vector,q=0*/
      else
      {   if(q<0.)
         {
            m=na;
    		/* in algol, was -q, with xi=0 . 
    		EISPACK commment says this alternate choice
    		was to make last eigenvector component imag.
    		so that eigenvector matrix triangular*/
            if(fabs(h INDEX(en,na))>fabs(h INDEX(na,en)))
            {
               h INDEX(na,en)=-(h INDEX(en,en) -p)/h INDEX(en,na);
               h INDEX(na,na)= q/h INDEX(en,na);
            }
            else
            {
		       xr=0.;
               xi=- h INDEX(na,en);
               yr= h INDEX(na,na)-p;
               yi=q;
               if(fabs(yr)>fabs(yi))
               {
                  t=yi/yr;
                  yr+=t*yi;
                  zr=(xr+t*xi)/yr;
                  zi=(xi-t*xr)/yr;
               }
               else
               {
                  t=yr/yi;
                  yi+=t*yr;
                  zr=(t*xr+xi)/yi;
                  zi=(t*xi-xr)/yi;
               }
               h INDEX(na,na)=zr;
               h INDEX(na,en)=zi;
            }
            h  INDEX(en,na)=0.;
            h INDEX(en,en) =1.;
            for(i=na-1;i>=0;i--)
            {
               w= h INDEX(i,i)-p;
               /* caveat- in algol, the assignments to ra,sa
               below were switched.  believe its bug fix
               in EISPACK */
               sa= h INDEX(i,en);
               ra=0.;
               for (j=m;j<=na;j++)
               {
                  ra+= h INDEX(i,j)*h INDEX(j,na);
                  sa+= h INDEX(i,j)*h INDEX(j,en);
               }
               if(wi[i]<0.)
               {
                  z=w;
                  r=ra;
                  s=sa;
               }
               else
               {
                  m=i;
                  if(wi[i]==0.) 
                  {
                     /*cdiv*/
                     xr=-ra;
                     xi=-sa;
                     yr=w;
                     yi=q;
                     if(fabs(yr)>fabs(yi) )
                     {
                        t=yi/yr;
                        yr+=t*yi;
                        zr=(xr+t*xi)/yr;
                        zi=(xi-t*xr)/yr;
                     }
                     else
                     {
                        t=yr/yi;
                        yi+=t*yr;
                        zr=(t*xr+xi)/yi;
                        zi=(t*xi-xr)/yi;
                     }
                     h INDEX(i,na)=zr;
                     h INDEX(i,en)=zi;
                  }
                  else
                  {
                     x=h INDEX(i,i+1);
                     y=h INDEX(i+1,i);
                     vi=wi[i];
                     t=(wr[i]-p);
                     vr=t*t+vi*vi-q*q;
                     vi=2.0*q*t;
                     if(vr==0. && vi==0.)
                     {
                        vr=macheps*norm*
                          (fabs(w)+fabs(q)+fabs(x)+fabs(y)+fabs(z) );
                     }/*zero vr and vi*/
                        /*cdiv*/
                        xr= x*r-z*ra+q*sa;
                        xi=s*x-z*sa-q*ra;
                        yr=vr;
                        yi=vi;
                        if(fabs(yr)>fabs(yi) )
                        {
                           t=yi/yr;
                           yr+=t*yi;
                           zr=(xr+t*xi)/yr;
                           zi=(xi-t*xr)/yr;
                        }
                        else
                        {
                           t=yr/yi;
                           yi+=t*yr;
                           zr=(t*xr+xi)/yi;
                           zi=(t*xi-xr)/yi;
                        }
                        h INDEX(i,na)=zr;
                        h INDEX(i,en)=zi;
                     if(fabs(x)>(fabs(z)+fabs(q)))
                     {
                        h INDEX(i+1,na)=
                          (-ra-w*h INDEX(i,na)+q*
                          h INDEX(i,en))/x;
                        h INDEX(i+1,en)=
                          (-sa-w*h INDEX(i,en)-q*
                          h INDEX(i,na) )/x;
                     }
                     else
                     {
                        /*cdiv*/
                        xr=-r-y* h INDEX(i,na);
                        xi=-s-y*h INDEX(i,en) ;
                        yr=z;
                        yi=q;
                        if(fabs(yr)>fabs(yi))
                        {
                           t=yi/yr;
                           yr+=t*yi;
                           zr=(xr+t*xi)/yr;
                           zi=(xi-t*xr)/yr;
                        }
                        else
                        {
                           t=yr/yi;
                           yi+=t*yr;
                           zr=(t*xr+xi)/yi;
                           zi=(t*xi-xr)/yi;
                        }
                        h INDEX(i+1,na)=zr;
                        h INDEX(i+1,en)=zi;
                     }
                  }/* wi!=0*/
               }
            }/*for i*/

         }/*q<0. complex vector*/
      }/*end else*/  
   }/*end for*/
/* vector of roots*/
   DOFOR(i,n)
   {
      if(i<low || i>igh)
      {
         for(j=i;j<n;j++)
         {
            vecs INDEX(i,j)= h INDEX(i,j);
         }
      }

   }
   for(j=n-1;j>=low;j--)
   {
      m= min( j , igh) ;
	  DFFR(i,low,igh)
	  	{
		z=0.;DFFR(k,low,m) z += vecs INDEX(i,k) * h INDEX(k,j);
		vecs INDEX(i,j)=z;
	  	}
   }/*end for j loop*/
   return;
}


elmhes(colm,n,low,igh,a,intar)
int colm,n,low,igh, intar[];
double *a;
{
   int i,j,m,la,kp1,mm1,mp1;
   double x,y;
   int coln;
   coln=n;
   la= igh-1;
   kp1=low+1;
   if(la<kp1)return;
   DFFR(m,kp1,la)
   {
      mm1=m-1;
      x=0.;
      i=m;
      DFFR(j,m,igh)
      {
         if( fabs( a INDEX(j,mm1) ) > fabs(x ) )
         {
            x= a INDEX(j,mm1);
            i=j;
         }
      }
      intar[m]=i;
   if(i!=m)
   {
      DFFOR(j,mm1,n)
      {
         y= a INDEX(i,j);
         a INDEX(i,j) = a INDEX(m,j);
         a INDEX(m,j) = y;
      }
      DFFR( j,0,igh)
      {
         y= a INDEX(j,i);
         a INDEX(j,i) = a INDEX(j,m);
         a INDEX(j,m) = y;
      }
   }
   if(x!=0.)
   {
      mp1=m+1;
      DFFR(i,mp1,igh)
      {
         y= a INDEX(i,mm1);
         if (y!=0.)
         {
            y/=x;
            a INDEX(i,mm1) =y;
            DFFOR(j,m,n) a INDEX(i,j) -= y* a INDEX(m,j);
            DFFR(j,0,igh) a INDEX(j,m) += y* a INDEX(j,i);
         }
      }
   }
 }
}

eltran(coln,n,low,igh,a,intar,z)
int coln,low,igh,n,intar[];
double *a,*z;
{/*form matrix of accumulated transforms z*/
   int colu,i,j,k,l,kl,mn,mp,mp1;
/* set z(n x n) to identity matrix; */
   colu=n;
   DOFOR(i,n)
   {
      DOFOR(j,n) z IU(i,j) =0.;
      z IU(i,i) = 1.;
   }
   kl=igh-low-1;
   if(kl<1)return;
   DFFRR(i,(igh-1),(low+1))
   {
      j= intar[i];
      DFFR(k,i+1,igh) z IU(k,i) =a INDEX(k,i-1);
      if(i!=j)
      {
         DFFR(k,i,igh)
         {
            z IU(i,k) = z IU(j,k);
            z IU(j,k)  =0.;
         }
         z IU(j,i)=1.;
      }
   }

}

/* not used as hqr2 does it
elmbak(colu,n,m,low,igh,a,intar,z)
int colu,n,low,igh,m,intar[];
double *z,*a;
{
   int coln,i,j,la,l,mm,mp,kp1,mp1;
   double x;
   if(m<0)return;
   la=igh-1;
   kp1=low+1;
   coln=n;
   if(la<kp1)return;
   DFFRR(mp,l-1,kp1)
   {
      mp1=mp+1;
      for (i=mp1;i<=igh;i++)
      {
         x= a IU(i,mp-1);
         if(x!=0.0)
         {
            DOFOR(j,m)
            z INDEX(i,j)+=x*z INDEX(mp,j);
         }
      }
      i=intar[mp];
      if(i!=mp)
      {
         DOFOR(j,m)
         {
            x= z INDEX(i,j);
            z INDEX(i,j) = z INDEX(mp,j);
            z INDEX(mp,j) = x;
         }
      }
   }
}
*/
   balbak(coln,n,low,igh,scale,m,z)
   int coln,n,low,igh; double scale[],*z;
   {/* m is number of eigenvectors to transform*/
      int i,j,k,l;
      double s;
      if(m==0)return;
      if(igh!=low)
      {
         DFFR(i,low,igh)
         {
            s=scale[i];
            DOFOR(j,m) z INDEX(i,j) *=s; 
         }
      }
      DOFOR(l,n)
      {
         i=l;
         if(i<low || i>igh)
         { /* l=0 i=low-1 l=low-1 i=0*/
            if(i<low)i=low-l-1;
            k=scale[i];
            if(k!=i)
				{
            	DOFOR(j,m)
            		{
               		s= z INDEX(i,j);
               		z INDEX(i,j)=z INDEX(k,j);
               		z INDEX(k,j)=s;
            		}
				}
         }
      }
      return;
   }



   int k,l,j,m,nn;

   exch(m,d,a) 
   int m;
   double d[],*a;
   {
      double f;
      int i,coln;
      coln=nn;
      d[m]=j;
/* note an integer is being stored in scale, d type double array*/
      if (j!=m)
      {
         DFFR(i,0,k)
         {
            f= a INDEX(i,j);
            a INDEX(i,j)= a INDEX(i,m);
            a INDEX(i,m)=f;
         }
 DFFOR(i,l,nn)
         {
            f= a INDEX(j,i);
            a INDEX(j,i)= a INDEX(m,i);
            a INDEX(m,i)=f;
         }
      }
   }

   balance(coln,n,a,low,igh,scale)
   double scale[],*a;
   int coln,n,*low,*igh;
   {/* assumed matrix a has coln columns, working on n x n portion */
      double radix=2.,b2,bi,b2i,r,c,b,s,f,g;
      int i,kt,noconv,lim;
      l=0;
      k=n-1;
      nn=n;
      lim= max(20,n*2);
      b=radix;
      b2=radix*radix;
      bi=1./b;
      b2i=1./b2;
      kt=0;
l1:
      DFFRR(j,k,0)
      {
         r=0.;
         DFFR(i,0,k)
         {
            if(i!=j)
            {
               r+=fabs( a INDEX(j,i) );
            }
         }
         if(r==0.)
         {
            exch(k,scale,a);
            k--;
            kt++;
            if(kt<lim)goto l1;
            else
            	{
            	exit(0);
            	}
         }
      }
      kt=0;
l2:
      DFFR(j,l,k)
      {
         c=0.;
         DFFR(i,l,k)
         {
            if(i!=j) c+=fabs(a INDEX(i,j) );
         }
         if (c==0.)
         {
            exch(l,scale,a);
            l++;
            kt++;
            if(kt<lim)goto l2;
            else
            	{
            	exit(0);
            	}
         }
      }
      *low=l;
      *igh=k;
      DFFR(i,l,k) scale[i]=1.;
      kt=0;
iteration:
      noconv=0;
      DFFR(i,l,k)
      {
         r=0.;
         c=0.;
         DFFR(j,l,k)
         {
            if (i!=j)
            {
               c+=fabs( a INDEX(j,i) );
	       r+=fabs( a INDEX(i,j) );
	    }
	  }
            g=r/b;
            f=1.;
	    s=c+r;
	    if(c==0. || r==0.) continue;
	    /* above if added in Fortran avoids infinite loops*/
            l3: if(c<g)
	    {
               f*=b;c*=b2;goto l3;
            }
            g=r*b;
            l4: if(c>=g)
	    {
	       f*=bi;c*=b2i;goto l4;
            }
            if( (c+r)/f < .95*s)
            {
               g=1./f;
               scale[i]*=f;
               noconv=1;
               DFFOR(j,l,n) a INDEX(i,j) *= g;
               DFFR(j,0,k)  a INDEX(j,i) *= f;
            }
         }/* do i */
         kt++;
         if(noconv && kt>lim)
         		{
         		printf(" balance iteration limit\n");
         		exit(0);
         		}
	 if (noconv)goto iteration;
   }

pnormwr(coln,n,z,wr,wi)
double *z,wr[],wi[];
int coln,n;
{/* normalize as in wilinson & Reinsch with largest element=1*/
int i,j,k;
double  t,xr,xi,yi,yr,zr,zi,emax,ei;
printf(" un normalized matrix:\n");
printmd(z,coln,n,n,n);

DOFOR(i,n)
	{
	if(wi[i]==0.)
		{
		emax=0.;
		DOFOR(j,n)
			{
			if ( fabs(z INDEX(j,i))>fabs(emax) )
							emax=z INDEX(j,i);
			}
		if(emax!=0.)
			{
			emax=1./emax;
			DOFOR(j,n)
				{
				z INDEX(j,i) *=emax;
				}
			}
		}
	else /*complex eigenvector*/
		{
		emax=0.;
		k=0;
		DOFOR(j,n)
			{
			xr= z INDEX(j,i)	; yr=z INDEX(j,i+1);
			xr= sqrt(xr*xr+yr*yr);
			if(xr>emax) 
				{
				emax=xr;
				k=j;
				}
			}
		emax=z INDEX(k,i);
		ei=z INDEX(k,i+1);	

		DOFOR(j,n)
			{
			yr=emax;
			yi=ei;
			xr=z INDEX(j,i);
			xi=z INDEX(j,i+1);
                        if(fabs(yr)>fabs(yi) )
                        {
                           t=yi/yr;
                           yr+=t*yi;
                           zr=(xr+t*xi)/yr;
                           zi=(xi-t*xr)/yr;
                        }
                        else
                        {
                           t=yr/yi;
                           yi+=t*yr;
                           zr=(t*xr+xi)/yi;
                           zi=(t*xi-xr)/yi;
                        }
			z INDEX(j,i)=zr;
			z INDEX(j,i+1)=zi;
			}			
		i++;
		}			
	}	/* DOFOR i loop*/
printf(" normalized matrix of eigenvectors:\n");
printmd(z,coln,n,n,n);

}
/* specify matrices by row.*/
double et[4][4]={1.,2.,3.,5.,2.,4.,1.,6.,1.,2.,-1.,3.,2.,0.,1.,3.};
double ev[4][4]={3.,1.,2.,5.,2.,1.,3.,-1.,0.,4.,1.,1.,0.,0.,2.,1.};
double ex[3][3]={2.,4.,4.,0.,3.,1.,0.,1.,3.};
double ey[3][3]={5.,-4.,-7.,-4.,2.,-4.,-7.,-4.,5.};

main(argc,argv)
int argc;
char **argv;
{/*test driver*/
double wr[4],wi[4],z[16],d[4];
float mach;
int i,j,iwork[4],cnt[4],bal;
elmhes(4,4,0,3,et,iwork);
printf(" elmhes test\n");
printmd(et,4,4,4,4);
printf(" enter macheps ");scanf("%e",&mach);
macheps=mach;
printf(" macheps=%e\n",macheps);
printf(" eigenv test\n");
bal=1;
eigenvv(4,4,ev,iwork,d,z,wr,wi,bal,cnt);
DOFOR(i,4)printf(" eigenvalue real= %e imag=%e it=%d\n",wr[i],wi[i],cnt[i]);
printmd(z,4,4,4,4);
pnormwr(4,4,z,wr,wi);
printf(" eigenv test 2\n");
bal=0;
eigenvv(3,3,ex,iwork,d,z,wr,wi,bal,cnt);
DOFOR(i,3)printf(" eigenvalue real= %e imag=%e it=%d\n",wr[i],wi[i],cnt[i]);
printmd(z,3,3,3,3);
pnormwr(3,3,z,wr,wi);
printf(" eign test 3\n");
bal=1;
eigenvv(3,3,ey,iwork,d,z,wr,wi,bal,cnt);
DOFOR(i,3)printf(" eigenvalue real= %e imag=%e it=%d\n",wr[i],wi[i],cnt[i]);
printmd(z,3,3,3,3);
pnormwr(3,3,z,wr,wi);
}



