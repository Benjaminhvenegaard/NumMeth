#include "ftoc.h"

/*
routines for linear systems processing via LU factorization
(from "More C Tools for Scientists and Engineers" by L. Baker)

PURPOSE: 

perform LU factorization

CONTENTS:

lufact(a,coln,n,pivot,info)
	Factor an n by n matrix contained in type float array a 
	which is dimensioned a[m][coln].  pivot is an integer array
	of size n which will contain pivoting information to be used
	by later routines.  info is pointer to an integer which returns 
	0 if all is well, otherwise the row in which problems occurred.

DEPENDENCIES:

requires header file ftoc.h and lus.c routines

Modified from LU.C for improved access of matrix values
*/

lufact (a,coln,n,pivot,info)
float *a;
int coln,n,*pivot,*info;
{
    int i,j,k,l,kp1,nm1,last,*piv,ip,kp;
    float t,q;
    *info=0;
    nm1=n-1;
	piv=pivot;
    DOFOR(k,n)*(piv++)=k;
    if (nm1>=1)
    {/*nontrivial pblm*/
        DOFOR(k,nm1)
        {
            kp1=k+1;
            /*partial pivoting ROW exchanges-search over column*/
            /* in FORTRAN, the increment would be 1 not n in ismax call*/
			t=0.;
			l=k;
			for(i=kp1;i<n;i++)
				{
 				q=abs( a INDEX(i ,k));
 				if(q>t)
 					{
 					t=q;
 					l=i;
 					}
				}      
			     pivot [k]=l;
            if (a INDEX(l,k)!=0.)
            {/*nonsingular pivot found*/
                if(l!=k)
                {/*interchange needed*/
				for(i=k;i<n;i++)
					{
                    t=a INDEX(l,i);
                    a INDEX(l,i)=a INDEX(k,i);
                    a INDEX(k,i)=t;
                    }
                }
				/* elementary row op */
				q=1./a INDEX(k,k);
                for(i=kp1;i<n;i++)
				{
                  t=   -q*a INDEX(i,k);
                  a INDEX(i,k) = t;
				  for(j=kp1;j<n;j++)/*column*/
				  	{
                    a INDEX(i,j) += t * a INDEX(k,j);
 			       }
          	   }
            }
                else /*pivot singular*/
                { *info=k;}
        }/*main loop over k*/
    }/*if nontrivial*/
        pivot [nm1]=nm1;
        if (a INDEX(nm1,nm1) ==0.0)*info=nm1;
        return;
}

