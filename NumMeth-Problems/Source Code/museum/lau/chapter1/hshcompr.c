void hshcomprd(int i, int ii, int l, int u, int j, float **ar,
					float **ai,	float **br, float **bi, float t)
{
	void elmcomcol(int, int, int, int, float **, float **,
						float **, float **, float, float);
	float tammat(int, int, int, int, float **, float **);

	for (; l<=u; l++)
		elmcomcol(i,ii,l,j,ar,ai,br,bi,
					(-tammat(i,ii,j,l,br,ar)-tammat(i,ii,j,l,bi,ai))/t,
					(tammat(i,ii,j,l,bi,ar)-tammat(i,ii,j,l,br,ai))/t);
}
