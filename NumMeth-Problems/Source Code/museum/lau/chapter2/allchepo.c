void allchepol(int n, float x, float t[])
{
	int i;
	float t1,t2,h,x2;

	if (n == 0) {
		t[0]=1.0;
		return;
	}
	if (n == 1) {
		t[0]=1.0;
		t[1]=x;
		return;
	}
	t[0]=t1=1.0;
	t[1]=t2=x;
	x2=x+x;
	for (i=2; i<=n; i++) {
		t[i]=h=x2*t2-t1;
		t1=t2;
		t2=h;;
	}
}
