
/* 
test driver for linear system solver:
	 LU factorization and related routines
from Handbook of C tools for Scientists and Engineers by L. Baker

tests LU factorization, solution of ordinary and transposed
systems, calculation of inverse matrix and determinant

*/
/*#include "libc.h"
#include "math.h"
*/

#include "ftoc.h"

main (argc,argv) int argc; char **argv;
{
    float a[3][3],b[3],c[3][3];
    int i,j,pivot[3],n,info,coln;float work[3];
    double det(),deti(),determ;
/* simultaneous equation test fix*/
    n=coln=3;
    b[0]=5.;
    b[1]=3.;
    b[2]=1.;
    a[0][0]=2.;a[0][1]=3.;a[0][2]=-1.;
    a[1][0]=4.;a[1][1]=4.;a[1][2]=-3.;
    a[2][0]=-2.; a[2][1]=3.; a[2][2]=-1.;
/* ans 1.,2.,3. */
    printf(" lufact coln=%d,n=%d\n",coln,n);
    lufact(a,coln,n,pivot,&info);
    printf(" lufact info=%d %d %d %d\n",info,pivot[0],pivot[1],pivot[2]);
    printm(a,3,3,3,3);
/* packed LU:  4 4 -3
	       -.5 5 -2.5
.5 .2  1. */
	determ=det(a,pivot,coln);
	printf(" determinant=%f \n",determ);
	determ=deti(a,pivot,coln,&i);
	printf(" determinant= %f x 10^ %d\n",determ,i);
    backsub(a,coln,n,pivot,b);
	printf("\n solution to linear system:");
    printf(" ans= %f %f %f\n",b[0],b[1],b[2]);
/* transposed system */
    b[0]=5.;
    b[1]=3.;
    b[2]=1.;
	backt(a,coln,n,pivot,b);
	printf("\n solution to transposed system:");
    printf(" ans= %f %f %f\n",b[0],b[1],b[2]);
/*inverse*/
	printf("\n LU factors as stored in a:");
    printm(a,3,3,3,3);
/* .25 0. -.25
   .5  -.2  .1
1. -.6   -.2  is inverse*/
    invm(a,coln,n,pivot,work);
    printf("\n inverse of a:");
    printm(a,3,3,3,3);
/* check transposed system solution */
    b[0]=5.;
    b[1]=3.;
    b[2]=1.;
    a[0][0]=2.;a[1][0]=3.;a[2][0]=-1.;
    a[0][1]=4.;a[1][1]=4.;a[2][1]=-3.;
    a[0][2]=-2.; a[1][2]=3.; a[2][2]=-1.;
    lufact(a,coln,n,pivot,&info);
    backsub(a,coln,n,pivot,b);
    printf("\n check transposed system\n ans= %f %f %f\n",b[0],b[1],b[2]);

    exit(0);
}
