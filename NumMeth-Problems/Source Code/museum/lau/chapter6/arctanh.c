#include <float.h>
#include <math.h>

float arctanh(float x)
{
	float logoneplusx(float);
	float ax;

	if (fabs(x) >= 1.0)
		return ((x > 0.0) ? FLT_MAX : -FLT_MAX);
	else {
		ax=fabs(x);
		return ((x == 0.0) ? 0.0 :	((x > 0.0) ?
					0.5*logoneplusx(2.0*ax/(1.0-ax)) :
					-0.5*logoneplusx(2.0*ax/(1.0-ax))));
	}
}
