#include <stdio.h>
void main ()
{
	float **allocate_real_matrix(int, int, int, int);
	void free_real_matrix(float **, int, int, int);
	void decsol(float **, int, float [], float []);
	int i,j;
	float **a,b[5],aux[4];

	a=allocate_real_matrix(1,4,1,4);
	for (i=1; i<=4; i++) {
		for (j=1; j<=4; j++) a[i][j]=1.0/(i+j-1);
		b[i]=a[i][3];
	}
	aux[2]=1.0e-5;
	decsol(a,4,aux,b);
	printf("Solution: %e  %e  %e  %e\n",b[1],b[2],b[3],b[4]);
	printf("Sign(Det) =%3.0f\nNumber of eliminations =%3.0f\n",
			aux[1],aux[3]);
	free_real_matrix(a,1,4,1);
}

