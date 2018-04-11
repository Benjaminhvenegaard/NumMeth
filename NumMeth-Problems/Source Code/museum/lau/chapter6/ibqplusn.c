void ibqplusn(float x, float p, float q, int nmax, float eps,
					float i[])
{
	void ixqfix(float, float, float, int, float, float []);
	void ixpfix(float, float, float, int, float, float []);
	int n;

	if (x == 0.0 || x == 1.0)
		for (n=0; n<=nmax; n++) i[n]=x;
	else {
		if (x <= 0.5)
			ixpfix(x,p,q,nmax,eps,i);
		else {
			ixqfix(1.0-x,q,p,nmax,eps,i);
			for (n=0; n<=nmax; n++) i[n]=1.0-i[n];
		}
	}
}
