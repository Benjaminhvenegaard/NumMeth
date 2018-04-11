void gsswts(int n, float zer[], float b[], float c[], float w[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void allortpol(int, float, float [], float [], float []);
	int j,k;
	float s,*p;

	p=allocate_real_vector(0,n-1);
	for (j=1; j<=n; j++) {
		allortpol(n-1,zer[j],b,c,p);
		s=0.0;
		for (k=n-1; k>=1; k--) s=(s+p[k]*p[k])/c[k];
		w[j]=1.0/(1.0+s);
	}
	free_real_vector(p,0);
}
