#include <stdio.h>
void main ()
{
	void sincosint(float, float *, float *);
	void sincosfg(float, float *, float *);
	float si,ci,f,g;
	sincosint(1.0,&si,&ci);
	sincosfg(1.0,&f,&g);
	printf("SINCOSINT and SINCOSFG deliver:\n");
	printf(" SI(1) = %+e   CI(1) = %+e\n"
			"  F(1) = %+e    G(1) = %+e\n",si,ci,f,g);
}

