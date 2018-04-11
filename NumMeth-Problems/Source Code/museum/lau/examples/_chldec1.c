#include <math.h>
#include <stdio.h>
void main ()
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void chlsol1(float [], int, float []);
	void chlinv1(float [], int);
	int i,j,jj;
	float *pascal1,*b,*aux;

	pascal1=allocate_real_vector(1,((4+1)*4)/2);
	b=allocate_real_vector(1,4);
	aux=allocate_real_vector(2,3);

	jj=1;
	for (j=1; j<=4; j++) {
		pascal1[jj]=1.0;
		for (i=2; i<=j; i++)
			pascal1[jj+i-1] = (i == j) ?
					pascal1[jj+i-2]*2.0 : pascal1[jj+i-2]+pascal1[jj+i-j];
		b[j]=pow(2.0,j);
		jj += j;
	}
	aux[2]=1.0e-11;
	chldec1(pascal1,4,aux);
	if (aux[3] == 4) {
		chlsol1(pascal1,4,b);
		chlinv1(pascal1,4);
	} else
		printf("Matrix not positive definite");
	printf("Solution with CHLDEC1 and CHLSOL1:\n %e  %e  %e  %e\n",
			b[1],b[2],b[3],b[4]);
	printf("\nInverse matrix with CHLINV1:\n");
	for (i=1; i<=4; i++) {
		for (j=1; j<=4; j++)
			if (j < i)
				printf("           ");
			else
				printf("%11.5f",pascal1[((j-1)*j)/2+i]);
		printf("\n");
	}

	free_real_vector(pascal1,1);
	free_real_vector(b,1);
	free_real_vector(aux,2);
}

