void commul(float ar, float ai, float br, float bi, float *rr, float *ri)
{
	*rr=ar*br-ai*bi;
	*ri=ar*bi+ai*br;
}
