void inimatd(int lr, int ur, int shift, float **a, float x)
{
	for (; lr<=ur; lr++) a[lr][lr+shift]=x;
}
