#include <math.h>

void quanewbnd(int n, int lw, int rw,
					float x[], float f[], float jac[],
					int (*funct)(int, int, int, float[], float[]),
					float in[], float out[])
{
	float *allocate_real_vector(int, int);
	void free_real_vector(float *, int);
	float vecvec(int, int, int, float [], float []);
	void elmvec(int, int, int, float [], float [], float);
	void mulvec(int, int, int, float [], float [], float);
	void dupvec(int, int, int, float [], float []);
	void decsolbnd(float [], int, int, int, float [], float []);
	int l,it,fcnt,fmax,err,b,i,j,k,r,m;
	float macheps,reltol,abstol,tolres,nd,mz,res,*delta,mul,crit,
			*pp,*s,aux[6],*lu;

	delta=allocate_real_vector(1,n);
	nd=0.0;
	macheps=in[0];
	reltol=in[1];
	abstol=in[2];
	tolres=in[3];
	fmax=in[4];
	mz=macheps*macheps;
	it=fcnt=0;
	b=lw+rw;
	l=(n-1)*b+n;
	b++;
	res=sqrt(vecvec(1,n,0,f,f));
	err=0;
	while (1) {
		if (err != 0 || (res < tolres &&
			sqrt(nd) < sqrt(vecvec(1,n,0,x,x))*reltol+abstol)) break;
		it++;
		if (it != 1) {
			/* update jac */
			pp=allocate_real_vector(1,n);
			s=allocate_real_vector(1,n);
			crit=nd*mz;
			for (i=1; i<=n; i++) pp[i]=delta[i]*delta[i];
			r=k=1;
			m=rw+1;
			for (i=1; i<=n; i++) {
				mul=0.0;
				for (j=r; j<=m; j++) mul += pp[j];
				j=r-k;
				if (fabs(mul) > crit) elmvec(k,m-j,j,jac,delta,f[i]/mul);
				k += b;
				if (i > lw)
					r++;
				else
					k--;
				if (m < n) m++;
			}
			free_real_vector(pp,1);
			free_real_vector(s,1);
		}
		/* direction */
		lu=allocate_real_vector(1,l);
		aux[2]=macheps;
		mulvec(1,n,0,delta,f,-1.0);
		dupvec(1,l,0,lu,jac);
		decsolbnd(lu,n,lw,rw,aux,delta);
		free_real_vector(lu,1);
		if (aux[3] != n) {
			err=3;
			break;
		} else {
			elmvec(1,n,0,x,delta,1.0);
			nd=vecvec(1,n,0,delta,delta);
			/* evaluate */
			fcnt += n;
			if (!((*funct)(n,1,n,x,f))) {
				err=2;
				break;
			}
			if (fcnt > fmax) err=1;
			res=sqrt(vecvec(1,n,0,f,f));
		}
	}
	out[1]=sqrt(nd);
	out[2]=res;
	out[3]=fcnt;
	out[4]=it;
	out[5]=err;
	free_real_vector(delta,1);
}
