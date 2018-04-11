#include <math.h>

void spherbessy(float x, int n, float y[])
{
	if (n == 0)
		y[0] = -cos(x)/x;
	else {
		int i;
		float yi,yi1,yi2;
		yi2 = y[0] = -cos(x)/x;
		yi1=y[1]=(yi2-sin(x))/x;
		for (i=2; i<=n; i++) {
			y[i] = yi = -yi2+(i+i-1)*yi1/x;
			yi2=yi1;
			yi1=yi;
		}
	}
}
