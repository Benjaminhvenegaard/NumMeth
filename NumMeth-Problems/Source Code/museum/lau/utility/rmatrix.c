#include <alloc.h>

float **allocate_real_matrix(int lr, int ur, int lc, int uc)
{
	/*  Allocates a real matrix of range [lr..ur][lc..uc].  */

	void system_error(char *);
	int i;
	float **p;

	p=(float **)malloc((unsigned) (ur-lr+1)*sizeof(float*));
	if (!p) system_error("Failure in allocate_real_matrix().");
	p -= lr;

	for (i=lr; i<=ur; i++){
		p[i]=(float *)malloc((unsigned) (uc-lc+1)*sizeof(float));
		if (!p[i]) system_error("Failure in allocate_real_matrix().");
		p[i] -= lc;
	}
	return p;
}
