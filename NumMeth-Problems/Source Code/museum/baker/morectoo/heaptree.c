/*
indirect heap routines for priority queue, heapsort 
modified from heap.c to use a binary tree rep. instead 
of arrays for greater flexibility in storage allocation.

from More C Tools for Scientists and Engineers by L. Baker

*/

#include <stdio.h>
#include <stdlib.h>
/*#include "alloc.h"*/


int order,heapsizenow;

#include "heaptree.h"
/*#include "libc.h"
  */
/* 
main-various tests
pqconstruct-build a heap initial data
pqinsert-insert an element
pqremove-remove FIRST element
pqreplace-change first element 
pqheaps-heapsort
pqdelete-remove arbitrary element

auxiliary routines-
pqupheap move element toward root to maintain heap condition
pqdownheap move element away from root to maintain heap
compare <> comparisons
comparee <= and >= comparisons
*/
/* order= 0 largest first (top) highest priority first
  1 smallest first- alphabetic ordering
*/

struct element *root;

struct element *dad;/*inserting new element, dad will be its father*/

int compare(Key, Key);
int comparee(Key,Key);

void pqheaps(char []);
void pqconstruct(int );
struct element *itself(int);

void walkdown(struct element *newelement)
/* starting from root, walk down toward  empty last element in heap
  adding the new element into heap in proper fashion*/
{
int pow2,halfpow,id,kp,leftson,sonid,temporary;
struct element *son,*toinsert,*temp;
DATA *tempdata;
/*printf(" walkdown heapsizenow=%d\n",heapsizenow);*/
kp=heapsizenow;
toinsert=newelement;
id=kp;
leftson=-1;/*undefined*/
dad=NULL;/*dad should initially be NULL as we start at root*/
for(pow2=1;(id>>=1)>0;pow2<<=1) ;
/*printf(" initially pow2=%d\n",pow2);*/
id=kp-pow2;
/*sonid for debugging purposes */
sonid=1;/*root*/
halfpow=1;
son=root;
for(;halfpow>0;pow2>>=1)
	{
	if(!son)break;
/*	printf(" son=%g toinsert=%g\n",son->key,toinsert->key);*/
	if(comparee( son->key,toinsert->key))
		{
		/* exchan%ge son and toinsert, placin%g toinsert into tree and
		   removing son*/
		temporary= son->key;
		son->key=toinsert->key;
		toinsert->key=temporary;
		tempdata= son->data;/* a pointer is a pointer*/
		son->data=toinsert->data;
		toinsert->data=tempdata;
		}
/*	printf(" son=%c toinsert=%c\n",son->key,toinsert->key);*/
	halfpow=pow2>>1;
/*printf(" debug father %d %d %d %d %c\n",id,pow2,sonid,leftson,son->key);*/
	if( id>=halfpow)
		/*go to right*/
		{
		dad=son;
		leftson=0;
		son=son->right;
		id-=halfpow;
		sonid=(sonid<<1)+1;
		}
	else
		/*to left*/
		{
		dad=son;
		leftson=1;
		son=son->left;
		sonid<<=1;
		}
/*printf(" debug2 father %d %d %d %d\n",id,pow2,sonid,leftson);*/
	}/* end loop*/
toinsert->father=dad;
if(leftson)
	{
	dad->left=toinsert;
	}
else
	dad->right=toinsert;
toinsert->father=dad;
/*printf(" adding at sonid=%d lefson=%d kp=%d %c\n"
	,sonid,leftson,kp,toinsert->key);*/
}
/* construct heap from keyvector*/
/*
extern char keyvector[];
void pqconstruct(int heapsize)
{
struct element *newelmnt;
for(heapsizenow=1;heapsizenow<=heapsize;heapsizenow++)
	{
	newelmnt=(element *)malloc( ESIZE );
	newelmnt->left=NULL;
	newelmnt->right=NULL;
	newelmnt->father=NULL;
	newelmnt->data=NULL;
	newelmnt->key= (double)((int)keyvector[heapsizenow-1]);
	if(heapsizenow==1)
		{
		root=newelmnt;
		}
	else
		walkdown(newelmnt);
	}
heapsizenow--;
}
*/
struct element *father(int k,int *leftson,struct element *startat)
/*determine father of kth element, and if this element is left/rt child*/
/* goes down from root- is there a better way? */
/* k of root is zero (not 1) */
{
int pow2,halfpow,id,kp,sonid;
struct element *son;
if(!k)
	{
	*leftson=0;/*doesn't matter*/
	return 0;/* its root */
	}
kp=k+1;
id=kp;
for(pow2=1;(id>>=1)>0;pow2<<=1) ;
id=kp-pow2;
/*sonid for debugging purposes */
sonid=1;/*root*/
halfpow=1;
for(son=startat;halfpow>0;pow2>>=1)
	{
	halfpow=pow2>>1;
/*printf(" debug father %d %d %d %d %c\n",id,pow2,sonid,*leftson,dad->key);*/
	if(!halfpow)
		{
		return dad;
		}
	if( id>=halfpow)
		/*go to right*/
		{
		dad=son;
		*leftson=0;
		son=son->right;
		id-=halfpow;
		sonid=(sonid<<1)+1;
		}
	else
		/*to left*/
		{
		dad=son;
		*leftson=1;
		son=son->left;
		sonid<<=1;
		}
/*	
printf(" debug2 father %d %d %d %d dad=%c\n",id,pow2,sonid,*leftson,dad->key);
if(!son)printf(" returning as dad\n ");
*/
	if(!son)return dad;/*dad,leftson/rightson set*/
	}

/*never get here */
printf(" bad exit father sonid=%d lefson=%d kp=%d\n",sonid,*leftson,kp);
exit(0);
return NULL;/*pacify compiler*/
}

struct element *itself(int k)
{
/* k of root is assumed to be 0 not 1 */
struct element *dad;
int leftson;
dad=father(k,&leftson,root);
/*printf(" itself k=%d leftson=%d\n",k,leftson);*/
if(!dad)
	return root;
else if(leftson) return dad->left;
else return dad->right;
}


int comparee(Key a,Key b)
{/* for order =0 returns 1 if a<=b else 0
               1    "    0 if a<=b else 1
 order=0 for largest at top (sedgewick)
       1 for priority queue with key time (FCFS) */

return ( ((a<=b) ? 1-order : order)  );
}

int compare(Key a,Key b)
{/* for order =0 returns 1 if a<b else 0
               1    "    0 if a<b else 1 
 order=0 for largest at top 
       1 for priority queue with key time (FCFS) */

return ( ((a<b) ? 1-order : order ) );
}

void pqdownheap(struct element *ei)
/* move e down the heap as necessary */
{
Key childkey,ekey,temporary;
struct element *left,*right,*child,*temp,*e;
DATA *dataptr,*tdata;
e=ei;
while(1)
	{
	ekey=e->key;
	left=e->left;
	right=e->right;
	if (left && right)
		{ 
		if(comparee(left->key,right->key) )
					child=right;
		else
					child=left;
		}
	else if (left) child=left;
	else if(right) child=right;
	else return;
	childkey= child->key;
	if(comparee(ekey,childkey) ) /*exchange*/
			{
			tdata=child->data;
			child->data=e->data;
			e->data=tdata;
			temporary=child->key;
			child->key=e->key;
			e->key=temporary;
			e=child;/* move down*/
			}
	else return;/*done- no more exch nece*/
	}
}

void pqinsert(e) struct element *e;
{
if(!heapsizenow)
	{
	root=e;
	heapsizenow++;
	return;
	}
/*else*/
heapsizenow++;
walkdown(e);
}

void pqupheap(struct element *e)
{
/* this is only operation to use father pointer*/
/* it in turn is only used in the delete op*/
struct element *left,*right,*dad,*temp;
DATA *d;
Key v,temporary;
while(1)
	{
	if(!(e))return;
	v=e->key;
	dad=e->father;
	if(!dad)return;
	if(comparee( dad->key  , v))
		{/*swap e with dad*/
		d=dad->data;
		dad->data=e->data;
		e->data=d;
		temporary=dad->key;
		dad->key=e->key;
		e->key=temporary;
		}
	e=e->father;/*move up*/
	}
}

struct element *pqremove()          /*remove first element*/
{
struct element *remove,*oldlast,*dad;
int leftson;
remove= root;
--heapsizenow;
if(!heapsizenow)
	{
	remove=root;
	root=NULL;
	return(remove);
	}
dad=father(heapsizenow,&leftson,root);
if(leftson)
	{
	oldlast=dad->left;
	dad->left=NULL;
	}
else
	{
	oldlast=dad->right;
	dad->right=NULL;
	}
/*make oldlast new root*/
if(dad!=root)
	{
	oldlast->right=root->right;
	oldlast->left=root->left;
	oldlast->father=NULL;
	(root->left)->father=oldlast;
	(root->right)->father=oldlast;
	}
else if(heapsizenow==1)
	{/*were only two elements*/
	oldlast->father=NULL;
	root=oldlast;
	return (remove);
	}
else
	{
	/* fixed 3/93*/
	oldlast->left=root->left;
	(root->left)->father=oldlast;
	}
oldlast->father=NULL;
root=oldlast;
pqdownheap(root);
return (remove);
}

void pqheaps(char list[])                /*heapsort*/
/* for tree heap, better to output or construct linked list*/

{
struct element *b,*c,*temp;
int index,leftson;
Key key;
//DATA d;unused 
index=0;
/*  construct  heap  from initially  unordered  array */
/*pqconstruct();   */  /*  assume  pqconstruct  has  been  called already*/
/*printf("\n");*/
do	{/*exchange first, last */
	list[index++]=root->key;
	dad=father(--heapsizenow,&leftson,root);
	if(leftson)
		{
		b=dad->left;
		dad->left=NULL;
		}
	else
		{
		b=dad->right;
		dad->right=NULL;
		}
/* in-place sort makes no sense now, as heapsize decreases and
	elements d from heap */	
	root->data=b->data;
	root->key=b->key;
	pqdownheap(root);
	} while (heapsizenow>0); /* n=1 will exchange 1 with itself-no need*/
list[index]=root->key;
}

struct element *pqreplace( struct element *ev)
{
/* ev is placed into the priority queue, and then the root element removed-
if the new item is highest priority, the heap is unchanged and the
new item's index in the data array is return
 NOTE THAT EV MAY BE ALTERED!*/
ev->left=root;/* note that root is unchanged*/
ev->right=NULL;
pqdownheap(ev);
return (ev);
}

void pqdelete(struct element *item)
{
/*delete   item
*/
Key oldkey,newkey; int leftson,remove;
struct element *last,*dad,*child;
if(item==root)
	{ last=pqremove();
	  return;
	}
oldkey=item->key;
/*delete the item, replacing it with
 last element in heap*/
	dad=father(--heapsizenow,&leftson,root);
        if(!dad)
		{
                 printf(" warning- deleting  fatherless element\n");
                 exit(0);		 return;
		}
	if(leftson)
		{
		last=dad->left;
		dad->left=NULL;
		}
	else
		{
		last=dad->right;
		dad->right=NULL;
		}
if(item==last)
	{return;}
/*reuse dad as father of item*/
dad=item->father;
if(item== (dad->left) )
					{
						dad->left=last;
					}
else
					{
					dad->right=last;
					}
last->left=item->left;
last->right=item->right;
last->father=dad;
child=item->left;
if(child)
	child->father=last;
child=item->right;
if(child)
	child->father=last;
newkey=last->key; if(newkey==oldkey)return;
/* if key unchanged, finished*/
/*otherwise, move new element up or down heap as needed.*/
/* order 0 max on top- compare(a,b) true a,b-
if newkey is smaller (usual heap convention) want downheap*/
if (compare(newkey,oldkey))pqdownheap(last);
else pqupheap(last);
return;
}

