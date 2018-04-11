void jacobnbndf(int n, int lw, int rw, float x[], float f[],
					float jac[], float (*di)(int),
					int (*funct)(int, int, int, float[], float[]))
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	int i,j,k,l,u,t,b,ll;
	float aid,stepi,*f1;

	l=1;
	u=lw+1;
	t=rw+1;
	b=lw+rw;
	for (i=1; i<=n; i++) {
		ll=l;
		f1=allocate_real_vector(ll,u);
		stepi=(*di)(i);
		aid=x[i];
		x[i]=aid+stepi;
		(*funct)(n,l,u,x,f1);
		x[i]=aid;
		k = i+((i <= t) ? 0 : i-t)*b;
		for (j=l; j<=u; j++) {
			jac[k]=(f1[j]-f[j])/stepi;
			k += b;
		}
		if (i >= t) l++;
		if (u < n) u++;
		free_real_vector(f1,ll);
	}
}
