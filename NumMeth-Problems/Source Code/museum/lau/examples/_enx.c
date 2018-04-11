#include <stdio.h>
void main ()
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void enx(float, int, int, float []);
	void nonexpenx(float, int, int, float []);
	int i;
	float b[2],*a;

	printf("ENX and NONEXPENX deliver:\n");
	a=allocate_real_vector(40,42);
	enx(1.1,40,42,a);
	for (i=40; i<=42; i++) printf(" E(%2d,1.1) = %e\n",i,a[i]);
	nonexpenx(50.1,1,1,b);
	printf("\nEXP(50.1)*E(1,50.1) = %e\n",b[1]);
	free_real_vector(a,40);
}

