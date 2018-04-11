#include <math.h>

void ixpfix(float x, float p, float q, int nmax, float eps,
				float i[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float incbeta(float, float, float, float);
	void forward(float, float, float, float, float, int, float []);
	void backward(float, float, float, float, int, float, float []);
	int m,mmax;
	float s,p0,i0,i1,iq0,iq1,*ip;

	m=floor(p);
	s=p-m;
	p0 = (s > 0.0) ? s : s+1.0;
	mmax = (s > 0.0) ? m : m-1;
	i0=incbeta(x,p0,q,eps);
	i1=incbeta(x,p0,q+1.0,eps);
	ip=allocate_real_vector(0,mmax);
	backward(x,p0,q,i0,mmax,eps,ip);
	iq0=ip[mmax];
	backward(x,p0,q+1.0,i1,mmax,eps,ip);
	iq1=ip[mmax];
	free_real_vector(ip,0);
	forward(x,p,q,iq0,iq1,nmax,i);
}
