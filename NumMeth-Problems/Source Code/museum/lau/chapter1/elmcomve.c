void elmcomveccol(int l, int u, int j, float ar[], float ai[],
						float **br, float **bi, float xr, float xi)
{
	void elmveccol(int, int, int, float [], float **, float);

	elmveccol(l,u,j,ar,br,xr);
	elmveccol(l,u,j,ar,bi,-xi);
	elmveccol(l,u,j,ai,br,xi);
	elmveccol(l,u,j,ai,bi,xr);
}
