void baksymtri1(float a[], int n, int n1, int n2, float **vec)
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float vecvec(int, int, int, float [], float []);
	void elmvec(int, int, int, float [], float [], float);
	int j,j1,k,ti,tj;
	float w,*auxvec;

	auxvec=allocate_real_vector(1,n);
	for (k=n1; k<=n2; k++) {
		for (j=1; j<=n; j++) auxvec[j]=vec[j][k];
		tj=j1=1;
		for (j=2; j<=n; j++) {
			ti=tj+j;
			w=a[ti];
			if (w < 0.0)
				elmvec(1,j1,tj,auxvec,a,vecvec(1,j1,tj,auxvec,a)*w);
			j1=j;
			tj=ti;
		}
		for (j=1; j<=n; j++) vec[j][k]=auxvec[j];
	}
	free_real_vector(auxvec,1);
}
