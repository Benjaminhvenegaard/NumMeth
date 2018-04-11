#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

unsigned char *getran(unsigned char *arr, size_t len);

#define MAXCHUNK 4096

void main(int argc, char **argv)
{
  int i, j, get, need;
  unsigned char arr[MAXCHUNK];

  if (argc == 1) {
    fprintf(stderr,"Syntax: %s number [number] [number] ... \n",argv[0]);
    exit(1);
  }

  get = 0;
  /* print out groups of numbers in the argument list */
  for (i = 1; i < argc; i++) {
    need = atoi(argv[i]);
    while (need > 0) {
      get = (need < MAXCHUNK ? need : get);
      getran(arr, get);
      for (j = 0; j < get; j++) {
	printf("%d\n",arr[j]);
      }
      need -= get;
    }
  }
}
