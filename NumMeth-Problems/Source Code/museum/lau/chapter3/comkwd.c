#include <math.h>

void comkwd(float pr, float pi, float qr, float qi,
				float *gr, float *gi, float *kr, float *ki)
{
	void commul(float, float, float, float, float *, float *);
	void comdiv(float, float, float, float, float *, float *);
	void comsqrt(float, float, float *, float *);
	float hr,hi;

	if (qr == 0.0 && qi == 0.0) {
		*kr = *ki = 0.0;
		*gr = pr*2.0;
		*gi = pi*2.0;
		return;
	}
	if (pr == 0.0 && pi == 0.0) {
		comsqrt(qr,qi,gr,gi);
		*kr = -(*gr);
		*ki = -(*gi);
		return;
	}
	if (fabs(pr) > 1.0 || fabs(pi) > 1.0) {
		comdiv(qr,qi,pr,pi,&hr,&hi);
		comdiv(hr,hi,pr,pi,&hr,&hi);
		comsqrt(1.0+hr,hi,&hr,&hi);
		commul(pr,pi,hr+1.0,hi,gr,gi);
	} else {
		comsqrt(qr+(pr+pi)*(pr-pi),qi+pr*pi*2.0,&hr,&hi);
		if (pr*hr+pi*hi > 0.0) {
			*gr = pr+hr;
			*gi = pi+hi;
		} else {
			*gr = pr-hr;
			*gi = pi-hi;
		}
	}
	comdiv(-qr,-qi,*gr,*gi,kr,ki);
}
