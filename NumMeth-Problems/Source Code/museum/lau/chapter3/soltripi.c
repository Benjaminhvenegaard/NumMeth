void soltripiv(float sub[], float diag[], float super[], int n,
					float aid[], int piv[], float b[])
{
	int i,n1;
	float bi,bi1,r,s,t;

	n1=n-1;
	for (i=1; i<=n1; i++) {
		if (piv[i]) {
			bi=b[i+1];
			bi1=b[i];
		} else {
			bi=b[i];
			bi1=b[i+1];
		}
		r=b[i]=bi/diag[i];
		b[i+1]=bi1-sub[i]*r;
	}
	r = b[n] /= diag[n];
	t = b[n1] -= super[n1]*r;
	for (i=n-2; i>=1; i--) {
		s=r;
		r=t;
		t = b[i] -= super[i]*r + ((piv[i]) ? aid[i]*s : 0.0);
	}
}
