#include <alloc.h>

float *allocate_real_vector(int l, int u)
{
	/*  Allocates a real vector of range [l..u].  */

	void system_error(char *);
	float *p;

	p=(float *)malloc((unsigned) (u-l+1)*sizeof(float));
	if (!p) system_error("Failure in allocate_real_vector().");
	return p-l;
}
