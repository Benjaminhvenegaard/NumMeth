#include <math.h>
#include <stdio.h>
void main ()
{
	float *allocate_real_vector(int, int);
	float **allocate_real_matrix(int, int, int, int);
	void free_real_vector(float *, int);
	void free_real_matrix(float **, int, int, int);
	void chldecsol2(float **, int, float [], float []);
	float chldeterm2(float **, int);
	void chldecinv2(float **, int, float []);
	int i,j;
	float determinant,**pascal2,*b,*aux;

	pascal2=allocate_real_matrix(1,4,1,4);
	b=allocate_real_vector(1,4);
	aux=allocate_real_vector(2,3);

	for (j=1; j<=4; j++) {
		pascal2[1][j]=1.0;
		for (i=2; i<=j; i++)
			pascal2[i][j] = (i == j) ?
					pascal2[i-1][j]*2.0 : pascal2[i][j-1]+pascal2[i-1][j];
		b[j]=pow(2.0,j);
	}
	aux[2]=1.0e-11;
	chldecsol2(pascal2,4,aux,b);
	if (aux[3] == 4)
		determinant=chldeterm2(pascal2,4);
	else
		printf("Matrix not positive definite");
	printf("Solution with CHLDECSOL2:\n %e  %e  %e  %e\n",
			b[1],b[2],b[3],b[4]);
	printf("\nDeterminant with CHLDETERM2: %e\n",determinant);
	for (j=1; j<=4; j++) {
		pascal2[1][j]=1.0;
		for (i=2; i<=j; i++)
			pascal2[i][j] = (i == j) ?
					pascal2[i-1][j]*2.0 : pascal2[i][j-1]+pascal2[i-1][j];
	}
	chldecinv2(pascal2,4,aux);
	printf("\nInverse matrix with CHLDECINV2:\n");
	for (i=1; i<=4; i++) {
		for (j=1; j<=4; j++)
			if (j < i)
				printf("           ");
			else
				printf("%11.5f",pascal2[i][j]);
		printf("\n");
	}

	free_real_matrix(pascal2,1,4,1);
	free_real_vector(b,1);
	free_real_vector(aux,2);
}

