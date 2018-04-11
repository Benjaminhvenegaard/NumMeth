void conjgrad(void (*matvec)(float [], float []), float x[],
					float r[], int l, int n, int (*goon)(int, float),
					int *iterate, float *norm2)
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float vecvec(int, int, int, float [], float []);
	void elmvec(int, int, int, float [], float [], float);
	int i;
	float a,b,prr,rrp,*p,*ap;

	p=allocate_real_vector(l,n);
	ap=allocate_real_vector(l,n);
	*iterate=0;
	do {
		if (*iterate == 0) {
			(*matvec)(x,p);
			for (i=l; i<=n; i++) p[i] = r[i] -= p[i];
			prr=vecvec(l,n,0,r,r);
		} else {
			b=rrp/prr;
			prr=rrp;
			for (i=l; i<=n; i++) p[i]=r[i]+b*p[i];
		}
		(*matvec)(p,ap);
		a=prr/vecvec(l,n,0,p,ap);
		elmvec(l,n,0,x,p,a);
		elmvec(l,n,0,r,ap,-a);
		*norm2=rrp=vecvec(l,n,0,r,r);
		(*iterate)++;
	} while ((*goon)(*iterate,*norm2));
	free_real_vector(p,l);
	free_real_vector(ap,l);
}
