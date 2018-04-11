float ortpolsym(int n, float x, float c[])
{
	int k,l;
	float r,s,h;

	if (n == 0) return (1.0);
	r=x;
	s=1.0;
	l=n-1;
	for (k=1; k<=l; k++) {
		h=r;
		r=x*r-c[k]*s;
		s=h;
	}
	return (r);
}
