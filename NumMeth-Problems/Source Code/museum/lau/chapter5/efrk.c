#include <float.h>
#include <math.h>

void efrk(float *t, float te, int m0, int m, float u[],
			float *sigma, float *phi, float *diameter,
			void (*derivative)(int, int, float, float[]),
			int *k, float *step, float r, float l, float beta[],
			int thirdorder, float tol,
			void (*output)(int, int, float, float,
								float [], float *, float *, float *,
								int, float *, int, int))
{
	int *allocate_integer_vector(int, int);
	float *allocate_real_vector(int, int);
	float **allocate_real_matrix(int, int, int, int);
	void free_integer_vector(int *, int);
	void free_real_vector(float *, int);
	void free_real_matrix(float **, int, int, int);
	void elmvec(int, int, int, float [], float [], float);
	void sol(float **, int, int [], float []);
	void dec(float **, int, float [], int []);
	int n,first,last,complex,change,*p,i,j,c1,c3;
	float theta0,thetanm1,h,b,b0,phi0,phil,cosphi,sinphi,eps,
			betar,*mu,*labda,*pt,*fac,*betac,*rl,**a,aux[4],
			dd,hstab,hstabint,c,temp,c2,e,b1,zi,cosiphi,siniphi,
			cosphil,*d,bb,mt,lt,tht;

	p=allocate_integer_vector(1,l);
	mu=allocate_real_vector(0,r+l-1);
	labda=allocate_real_vector(0,r+l-1);
	pt=allocate_real_vector(0,r);
	fac=allocate_real_vector(0,l-1);
	betac=allocate_real_vector(0,l-1);
	rl=allocate_real_vector(m0,m);
	d=allocate_real_vector(1,l);
	a=allocate_real_matrix(1,l,1,l);

	n=r+l;
	first=1;
	b0 = -1.0;
	betar=pow(beta[r],1.0/r);
	last=0;
	eps=FLT_EPSILON;
	phi0=phil=4.0*atan(1.0);
	do {
		/* stepsize */
		h=(*step);
		dd=fabs((*sigma)*sin(*phi));
		complex=(((l/2)*2 == l) && 2.0*dd > *diameter);
		if (*diameter > 0.0) {
			temp=(*sigma)*(*sigma)/((*diameter)*((*diameter)*0.25+dd));
			hstab=pow(temp,l*0.5/r)/betar/(*sigma);
		} else
			hstab=h;
		dd = (thirdorder ? pow(2.0*tol/eps/beta[r],1.0/(n-1))*
				pow(4.0,(l-1.0)/(n-1.0)) : pow(tol/eps,(1.0/r)/betar));
		hstabint=fabs(dd/(*sigma));
		if (h > hstab) h=hstab;
		if (h > hstabint) h=hstabint;
		if ((*t)+h > te*(1.0-(*k)*eps)) {
			last=1;
			h=te-(*t);
		}
		b=h*(*sigma);
		dd=(*diameter)*0.1*h;
		dd *= dd;
		if (h < (*t)*eps) break;
		change=((b0 == -1.0) ||
				((b-b0)*(b-b0)+b*b0*((*phi)-phi0)*((*phi)-phi0) > dd));
		if (change) {
			/* coefficient */
			b0=b;
			phi0=(*phi);
			if (b >= 1.0) {
				if (complex) {
					/* solution of complex equations */
					if (phi0 != phil) {
						/* elements of matrix */
						phil=phi0;
						cosphi=cos(phil);
						sinphi=sin(phil);
						cosiphi=1.0;
						siniphi=0.0;
						for (i=0; i<=l-1; i++) {
							c1=r+1+i;
							c2=1.0;
							for (j=l-1; j>=1; j-=2) {
								a[j][l-i]=c2*cosiphi;
								a[j+1][l-i]=c2*siniphi;
								c2 *= c1;
								c1--;
							}
							cosphil=cosiphi*cosphi-siniphi*sinphi;
							siniphi=cosiphi*sinphi+siniphi*cosphi;
							cosiphi=cosphil;
						}
						aux[2]=0.0;
						dec(a,l,aux,p);
					}
					/* right hand side */
					e=exp(b*cosphi);
					b1=b*sinphi-(r+1)*phil;
					cosiphi=e*cos(b1);
					siniphi=e*sin(b1);
					b1=1.0/b;
					zi=pow(b1,r);
					for (j=l; j>=2; j-=2) {
						d[j]=zi*siniphi;
						d[j-1]=zi*cosiphi;
						cosphil=cosiphi*cosphi-siniphi*sinphi;
						siniphi=cosiphi*sinphi+siniphi*cosphi;
						cosiphi=cosphil;
						zi *= b;
					}
					cosiphi=zi=1.0;
					siniphi=0.0;
					for (i=r; i>=0; i--) {
						c1=i;
						c2=beta[i];
						c3=((2*i > l-2) ? 2 : l-2*i);
						cosphil=cosiphi*cosphi-siniphi*sinphi;
						siniphi=cosiphi*sinphi+siniphi*cosphi;
						cosiphi=cosphil;
						for (j=l; j>=c3; j-=2) {
							d[j] += zi*c2*siniphi;
							d[j-1] -= zi*c2*cosiphi;
							c2 *=c1;
							c1--;
						}
						zi *= b1;
					}
					sol(a,l,p,d);
					for (i=1; i<=l; i++) beta[r+i]=d[l+1-i]*b1;
				} else {
					/* form beta */
					if (first) {
						/* form constants */
						first=0;
						fac[0]=1.0;
						for (i=1; i<=l-1; i++) fac[i]=i*fac[i-1];
						pt[r]=l*fac[l-1];
						for (i=1; i<=r; i++) pt[r-i]=pt[r-i+1]*(l+i)/i;
					}
					if (l == 1) {
						c=1.0-exp(-b);
						for (j=1; j<=r; j++) c=beta[j]-c/b;
						beta[r+1]=c/b;
					} else
						if (b > 40.0)
							for (i=r+1; i<=r+l; i++) {
								c=0.0;
								for (j=0; j<=r; j++)
									c=beta[j]*pt[j]/(i-j)-c/b;
								beta[i]=c/b/fac[l+r-i]/fac[i-r-1];
							}
						else {
							dd=c=exp(-b);
							betac[l-1]=dd/fac[l-1];
							for (i=1; i<=l-1; i++) {
								c=b*c/i;
								dd += c;
								betac[l-1-i]=dd/fac[l-1-i];
							}
							bb=1.0;
							for (i=r+1; i<=r+l; i++) {
								c=0.0;
								for (j=0; j<=r; j++)
									c=(beta[j]-((j < l) ? betac[j] : 0.0))*
											pt[j]/(i-j)-c/b;
								beta[i]=c/b/fac[l+r-i]/fac[i-r-1]+
											((i < l) ? bb*betac[i] : 0.0);
								bb *= b;
							}
						}
				}
			}
			labda[0]=mu[0]=0.0;
			if (thirdorder) {
				theta0=0.25;
				thetanm1=0.75;
				if (b < 1.0) {
					c=mu[n-1]=2.0/3.0;
					labda[n-1]=5.0/12.0;
					for (j=n-2; j>=1; j--) {
						c=mu[j]=c/(c-0.25)/(n-j+1);
						labda[j]=c-0.25;
					}
				} else {
					c=mu[n-1]=beta[2]*4.0/3.0;
					labda[n-1]=c-0.25;
					for (j=n-2; j>=1; j--) {
						c=mu[j]=c/(c-0.25)*beta[n-j+1]/beta[n-j]/
									((j < l) ? b : 1.0);
						labda[j]=c-0.25;
					}
				}
			} else {
				theta0=0.0;
				thetanm1=1.0;
				if (b < 1.0)
					for (j=n-1; j>=1; j--) mu[j]=labda[j]=1.0/(n-j+1);
				else {
					labda[n-1]=mu[n-1]=beta[2];
					for (j=n-2; j>=1; j--)
						mu[j]=labda[j]=beta[n-j+1]/beta[n-j]/
											((j < l) ? b : 1.0);
				}
			}
		}
		(*k)++;
		/* difference scheme */
		i = -1;
		do {
			i++;
			mt=mu[i]*h;
			lt=labda[i]*h;
			for (j=m0; j<=m; j++) rl[j]=u[j]+lt*rl[j];
			(*derivative)(m0,m,(*t)+mt,rl);
			if (i == 0 || i == n-1) {
				tht=((i == 0) ? theta0*h : thetanm1*h);
				elmvec(m0,m,0,u,rl,tht);
			}
		} while (i < n-1);
		(*t) += h;
		(*output)(m0,m,*t,te,u,sigma,phi,diameter,*k,step,r,l);
	} while (!last);
	free_integer_vector(p,1);
	free_real_vector(mu,0);
	free_real_vector(labda,0);
	free_real_vector(pt,0);
	free_real_vector(fac,0);
	free_real_vector(betac,0);
	free_real_vector(rl,m0);
	free_real_vector(d,1);
	free_real_matrix(a,1,l,1);
}

