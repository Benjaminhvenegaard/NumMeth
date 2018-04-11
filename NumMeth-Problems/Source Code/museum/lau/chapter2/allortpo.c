void allortpol(int n, float x, float b[], float c[], float p[])
{
	int k,k1;
	float r,s,h;

	if (n == 0) {
		p[0]=1.0;
		return;
	}
	r=p[1]=x-b[0];
	s=p[0]=1.0;
	k=1;
	for (k1=2; k1<=n; k1++) {
		h=r;
		p[k1]=r=(x-b[k])*r-c[k]*s;
		s=h;
		k=k1;
	}
}
