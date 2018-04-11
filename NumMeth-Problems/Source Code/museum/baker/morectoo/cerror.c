
cerror(a,b,eps)double eps; struct complex *a,*b;
{
struct complex dummy;
pdisp(a,eps,b,&dummy,20);
dummy.x=b->y;/* dummy=-i * b */
dummy.y=-b->x;
CTREAL((*b),dummy,.564189583);
}
