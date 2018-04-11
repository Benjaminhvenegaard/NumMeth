void forward(float x, float p, float q, float i0, float i1,
				int nmax, float i[])
{
	int m,n;
	float y,r,s;

	i[0]=i0;
	if (nmax > 0) i[1]=i1;
	m=nmax-1;
	r=p+q-1.0;
	y=1.0-x;
	for (n=1; n<=m; n++) {
		s=(n+r)*y;
		i[n+1]=((n+q+s)*i[n]-s*i[n-1])/(n+q);
	}
}
