void commatvec(int l, int u, int i, float **ar, float **ai,
					float br[], float bi[], float *rr, float *ri)
{
	float matvec(int, int, int, float **, float []);
	float mv;

	mv=matvec(l,u,i,ar,br)-matvec(l,u,i,ai,bi);
	*ri=matvec(l,u,i,ai,br)+matvec(l,u,i,ar,bi);
	*rr=mv;
}
