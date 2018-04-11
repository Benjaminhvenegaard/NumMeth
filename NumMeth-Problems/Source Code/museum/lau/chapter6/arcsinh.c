#include <math.h>

float arcsinh(float x)
{
	float logoneplusx(float);
	float y;

	if (fabs(x) > 1.0e10)
		return ((x > 0.0) ? 0.69314718055995+log(fabs(x)) :
									-0.69314718055995+log(fabs(x)));
	else {
		y=x*x;
		return ((x == 0.0) ? 0.0 :	((x > 0.0) ?
					logoneplusx(fabs(x)+y/(1.0+sqrt(1.0+y))) :
					-logoneplusx(fabs(x)+y/(1.0+sqrt(1.0+y)))));
	}
}
