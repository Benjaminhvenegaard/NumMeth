float symmatvec(int l, int u, int i, float a[], float b[])
{
	int k, m;
	float vecvec(int, int, int, float [], float []);
	float seqvec(int, int, int, int, float [], float []);

	m=(l>i) ? l : i;
	k=(m*(m-1))/2;
	return (vecvec(l, (i<=u) ? i-1 : u, k,b,a) + seqvec(m,u,k+i,0,a,b));
}
