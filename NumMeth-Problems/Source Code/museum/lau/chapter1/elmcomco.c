void elmcomcol(int l, int u, int i, int j, float **ar, float **ai,
					float **br, float **bi, float xr, float xi)
{
	void elmcol(int, int, int, int, float **, float **, float);

	elmcol(l,u,i,j,ar,br,xr);
	elmcol(l,u,i,j,ar,bi,-xi);
	elmcol(l,u,i,j,ai,br,xi);
	elmcol(l,u,i,j,ai,bi,xr);
}
