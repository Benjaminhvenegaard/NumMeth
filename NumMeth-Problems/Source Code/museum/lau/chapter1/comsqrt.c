#include <math.h>

void comsqrt(float ar, float ai, float *pr, float *pi)
{
	float br,bi,h,temp;

	if (ar == 0.0 && ai == 0.0)
		*pr = *pi = 0.0;
	else {
		br=fabs(ar);
		bi=fabs(ai);
		if (bi < br) {
			temp=bi/br;
			if (br < 1.0)
				h=sqrt((sqrt(temp*temp+1.0)*0.5+0.5)*br);
			else
				h=sqrt((sqrt(temp*temp+1.0)*0.125+0.125)*br)*2;
		} else {
			if (bi < 1.0) {
				temp=br/bi;
				h=sqrt((sqrt(temp*temp+1.0)*bi+br)*2)*0.5;
			} else {
				if (br+1.0 == 1.0)
					h=sqrt(bi*0.5);
				else {
					temp=br/bi;
					h=sqrt(sqrt(temp*temp+1.0)*bi*0.125+br*0.125)*2;
				}
			}
		}
		if (ar >= 0.0) {
			*pr=h;
			*pi=ai/h*0.5;
		} else {
			*pi = (ai >= 0.0) ? h : -h;
			*pr = bi/h*0.5;
		}
	}
}
