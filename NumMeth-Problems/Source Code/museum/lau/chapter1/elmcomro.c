void elmcomrowvec(int l, int u, int i, float **ar, float **ai,
						float br[], float bi[], float xr, float xi)
{
	void elmrowvec(int, int, int, float **, float [], float);

	elmrowvec(l,u,i,ar,br,xr);
	elmrowvec(l,u,i,ar,bi,-xi);
	elmrowvec(l,u,i,ai,br,xi);
	elmrowvec(l,u,i,ai,bi,xr);
}
