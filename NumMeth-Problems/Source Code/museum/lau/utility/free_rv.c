#include <alloc.h>

void free_real_vector(float *v, int l)
{
	/*  Frees a real vector of range [l..u].  */

	free((char*) (v+l));
}

