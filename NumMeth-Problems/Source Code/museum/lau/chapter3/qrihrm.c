int qrihrm(float **a, int n, float val[], float **vr, float **vi,
				float em[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	void hshhrmtri(float **, int, float [], float [], float [],
						float [], float [], float []);
	int qrisymtri(float **, int, float [], float [], float [], float []);
	void bakhrmtri(float **, int, int, int, float **,
						float **, float [], float []);
	int i,j;
	float *b,*bb,*tr,*ti;

	b=allocate_real_vector(1,n);
	bb=allocate_real_vector(1,n);
	tr=allocate_real_vector(1,n-1);
	ti=allocate_real_vector(1,n-1);
	hshhrmtri(a,n,val,b,bb,em,tr,ti);
	for (i=1; i<=n; i++) {
		vr[i][i]=1.0;
		for (j=i+1; j<=n; j++) vr[i][j]=vr[j][i]=0.0;
	}
	b[n]=bb[n]=0.0;
	i=qrisymtri(vr,n,val,b,bb,em);
	bakhrmtri(a,n,i+1,n,vr,vi,tr,ti);
	free_real_vector(b,1);
	free_real_vector(bb,1);
	free_real_vector(tr,1);
	free_real_vector(ti,1);
	return i;
}

