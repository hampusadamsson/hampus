#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main(){
  int *x = 12;
  printf("varibaeln X: %d\n",x);
  printf("adressen X : %d\n",&x);

  int *y = &x; 

  printf("varibaeln Y: %d\n",*y);
  printf("adressen Y : %d\n",y);

  *y= 100;

  printf("varibaeln Y: %d\n",*y);
  printf("adressen Y : %d\n",y);
  printf("varibaeln X: %d\n",&x);
  printf("adressen X : %d\n",x);


  return;
}
