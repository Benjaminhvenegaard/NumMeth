#include <math.h>

void comfouser2(int n, float theta, float ar[], float ai[],
					float *rr, float *ri)
{
	void comfouser(int, float, float [], float *, float *);
	float car,cai,sar,sai;

	comfouser(n,theta,ar,&car,&sar);
	comfouser(n,theta,ai,&cai,&sai);
	*rr=car-sai;
	*ri=cai+sar;
}
