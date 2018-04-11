void quanewbnd1(int n, int lw, int rw, float x[], float f[],
					int (*funct)(int, int, int, float[], float[]),
					float in[], float out[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float quanewbnd1t(float, int);
	void quanewbnd(int, int, int, float [], float [], float [],
						int (*)(int, int, int, float[], float[]),
						float [], float []);
	void jacobnbndf(int, int, int, float [], float [],
						float [], float (*)(int),
						int (*)(int, int, int, float[], float[]));
	int k;
	float *jac;

	jac=allocate_real_vector(1,(lw+rw)*(n-1)+n);
	(*funct)(n,1,n,x,f);
	k=(lw+rw)*(n-1)+n*2-((lw-1)*lw+(rw-1)*rw)/2;
	in[4] -= k;
	quanewbnd1t(in[5], 1);
	jacobnbndf(n,lw,rw,x,f,jac,quanewbnd1s,funct);
	quanewbnd(n,lw,rw,x,f,jac,funct,in,out);
	in[4] += k;
	out[3] += k;
	free_real_vector(jac,1);
}

float quanewbnd1s(int i)
{
	/* this function is used internally by QUANEWBND1 */

	float quanewbnd1t(float, int);

	return (quanewbnd1t(0.0,0));
}

float quanewbnd1t(float x, int i)
{
	/* this function is used internally by QUANEWBND1 */

	static float y;

	y = (i ? x : y);
	return (y);
}

