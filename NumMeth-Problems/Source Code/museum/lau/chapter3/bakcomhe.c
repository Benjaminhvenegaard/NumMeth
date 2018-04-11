void bakcomhes(float **ar, float **ai, float tr[], float ti[],
					float del[], float **vr, float **vi, int n,
					int n1, int n2)
{
	void hshcomprd(int, int, int, int, int, float **,
						float **, float **, float **, float);
	void comrowcst(int, int, int, float **, float **, float, float);
	int i,r,rm1;
	float h;

	for (i=2; i<=n; i++) comrowcst(n1,n2,i,vr,vi,tr[i],ti[i]);
	r=n-1;
	for (rm1=n-2; rm1>=1; rm1--) {
		h=del[rm1];
		if (h > 0.0) hshcomprd(r,n,n1,n2,rm1,vr,vi,ar,ai,h);
		r=rm1;
	}
}
