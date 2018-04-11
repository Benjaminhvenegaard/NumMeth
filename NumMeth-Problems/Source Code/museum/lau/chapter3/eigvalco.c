int eigvalcom(float **ar, float **ai, int n, float em[],
					float valr[], float vali[])
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void hshcomhes(float **, float **, int, float [], float [],
						float [], float [], float []);
	float comeucnrm(float **, float **, int, int);
	void eqilbrcom(float **, float **, int, float [], float [], int []);
	int valqricom(float **, float **, float [], int, float [],
						float [], float []);
	int i,*ind;
	float *d,*b,*del,*tr,*ti;

	ind=allocate_integer_vector(1,n);
	d=allocate_real_vector(1,n);
	b=allocate_real_vector(1,n);
	del=allocate_real_vector(1,n);
	tr=allocate_real_vector(1,n);
	ti=allocate_real_vector(1,n);
	eqilbrcom(ar,ai,n,em,d,ind);
	em[1]=comeucnrm(ar,ai,n-1,n);
	hshcomhes(ar,ai,n,em,b,tr,ti,del);
	i=valqricom(ar,ai,b,n,em,valr,vali);
	free_integer_vector(ind,1);
	free_real_vector(d,1);
	free_real_vector(b,1);
	free_real_vector(del,1);
	free_real_vector(tr,1);
	free_real_vector(ti,1);
	return i;
}

