float ortpol(int n, float x, float b[], float c[])
{
	int k,l;
	float r,s,h;

	if (n == 0) return (1.0);
	r=x-b[0];
	s=1.0;
	l=n-1;
	for (k=1; k<=l; k++) {
		h=r;
		r=(x-b[k])*r-c[k]*s;
		s=h;
	}
	return (r);
}
