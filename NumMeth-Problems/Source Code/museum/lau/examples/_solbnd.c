#include <stdio.h>
void main ()
{
	void decbnd(float [], int, int, int, float [], float [], int []);
	void solbnd(float [], int, int, int, float [], int [], float []);
	float determbnd(float [], int, int, int, int);
	int i,rowind[6];
	float band[14],mult[5],right[6],aux[6];

	for (i=1; i<=13; i++)
		band[i] = (((i+1)/3)*3 < i) ? 2.0 : -1.0;
	right[1]=right[5]=1.0;
	right[2]=right[3]=right[4]=0.0;
	aux[2]=1.0e-12;
	decbnd(band,5,1,1,aux,mult,rowind);
	if (aux[3] == 5) {
		solbnd(band,5,1,1,mult,rowind,right);
		printf("Delivers: %8.4f %8.4f %8.4f %8.4f %8.4f\n"
				"Determinant is  %e\n",right[1],right[2],right[3],
				right[4],right[5],determbnd(band,5,1,1,aux[1]));
	}
}

