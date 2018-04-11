#include <math.h>

void bessy(float x, int n, float y[])
{
	void bessy01(float, float *, float *);
	int i;
	float y0,y1,y2;

	bessy01(x,&y0,&y1);
	y[0]=y0;
	if (n > 0) y[1]=y1;
	x=2.0/x;
	for (i=2; i<=n; i++) {
		y[i]=y2=(i-1)*x*y1-y0;
		y0=y1;
		y1=y2;
	}
}
