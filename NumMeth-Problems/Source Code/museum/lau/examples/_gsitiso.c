#include <stdio.h>
void main ()
{
	float **allocate_real_matrix(int, int, int, int);
	void free_real_matrix(float **, int, int, int);
	void gssitisol(float **, int, float [], float []);
	int i,j;
	float **a,b[5],aux[14];

	a=allocate_real_matrix(1,4,1,4);
	for (i=1; i<=4; i++) {
		for (j=1; j<=4; j++) a[i][j]=840/(i+j-1);
		b[i]=a[i][3];
	}
	aux[2]=aux[10]=1.0e-5;
	aux[4]=8;
	aux[12]=5.0;
	gssitisol(a,4,aux,b);
	printf("Solution: %e  %e  %e  %e\n",b[1],b[2],b[3],b[4]);
	printf("Sign(Det) =%3.0f\nNumber of eliminations =%3.0f\n"
			"Max(abs(a[i,j])) = %e\nUpper bound growth = %e\n"
			"Norm last correction vector = %e\n"
			"Norm residual vector = %e\n",
			aux[1],aux[3],aux[5],aux[7],aux[11],aux[13]);
	free_real_matrix(a,1,4,1);
}

