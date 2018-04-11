void newton(int n, float x[], float f[])
{
	int k,i,im1;
	float xim1,fim1;

	im1=0;
	for (i=1; i<=n; i++) {
		fim1=f[im1];
		xim1=x[im1];
		for (k=i; k<=n; k++) f[k]=(f[k]-fim1)/(x[k]-xim1);
		im1=i;
	}
}
