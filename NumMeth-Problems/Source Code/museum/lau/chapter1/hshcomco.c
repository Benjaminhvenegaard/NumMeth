#include <math.h>

int hshcomcol(int l, int u, int j, float **ar, float **ai, float tol,
					float *k, float *c, float *s, float *t)
{
	void carpol(float, float, float *, float *, float *);
	float tammat(int, int, int, int, float **, float **);
	float vr, mod, h, arlj, ailj;

	vr=tammat(l+1,u,j,j,ar,ar)+tammat(l+1,u,j,j,ai,ai);
	arlj=ar[l][j];
	ailj=ai[l][j];
	carpol(arlj,ailj,&mod,c,s);
	if (vr > tol) {
		vr += arlj*arlj+ailj*ailj;
		h = *k = sqrt(vr);
		*t=vr+mod*h;
		if (arlj == 0.0 && ailj == 0.0)
			ar[l][j]=h;
		else {
			ar[l][j]=arlj + *c * *k;
			ai[l][j]=ailj + *s * *k;
			*s = - *s;
		}
		*c = - *c;
		return (1);
	} else {
		*k=mod;
		*t = -1.0;
		return (0);
	}
}
