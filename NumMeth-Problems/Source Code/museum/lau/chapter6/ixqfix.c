#include <math.h>

void ixqfix(float x, float p, float q, int nmax, float eps,
				float i[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float incbeta(float, float, float, float);
	void forward(float, float, float, float, float, int, float []);
	void backward(float, float, float, float, int, float, float []);
	int m,mmax;
	float s,iq0,iq1,q0,*iq;

	m=floor(q);
	s=q-m;
	q0 = (s > 0.0) ? s : s+1.0;
	mmax = (s > 0.0) ? m : m-1;
	iq0=incbeta(x,p,q0,eps);
	if (mmax > 0) iq1=incbeta(x,p,q0+1.0,eps);
	iq=allocate_real_vector(0,mmax);
	forward(x,p,q0,iq0,iq1,mmax,iq);
	backward(x,p,q,iq[mmax],nmax,eps,i);
	free_real_vector(iq,0);
}
