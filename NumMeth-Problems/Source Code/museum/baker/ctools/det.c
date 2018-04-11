/* two routines for finding determinants
(from "C Tools for Scientists and Engineers" by L. Baker)

PURPOSE: return value of determinant of a matrix

CONTENTS:
det() 
	Returns the determinant of a matrix factored in LU form.

deti() 
	Returns the determinant in the form  a*10**j where j
	is an integer.  It is useful if the determinant might be
	very large or small, resulting in overflows or underflows.

DEPENDENCIES:
ftoc.h header file
assumes lufact() has performed the matrix factorization
*/

#include <ftoc.h>

double dabs(x) double x;
{
    if(x>=0.)return x;
    return (-x);
}


double det(a,pivot,coln)
int pivot[],coln;
float *a;
{
    int i,sign;
    double d;
    sign=0;
    d=1.;
    for (i=0;i<coln;i++)
    {
        if(pivot[i]!=i)sign++;
        d*= a INDEX(i,i);
    }
    sign=sign-((sign>>1)<<1);
    if (sign) d=-d;
    return d;
}

double deti(a,pivot,coln,expon)
int pivot[],coln,*expon;
float *a;
{
    int i,sign;
    double d,dabs();
    sign=0;
    d=1.;
    *expon=0;
    for (i=0;i<coln;i++)
    {
        if(pivot[i]!=i)sign++;
        d*= a INDEX(i,i);
        if(dabs(d)>10.)
        {
            (*expon)++;
            d*=.1;
        }
        else if(dabs(d)<.1)
        {
            (*expon)--;
            d*=10.;
        }

    }
    sign=sign-((sign>>1)<<1);
    if (sign) d=-d;
    return d;
}
