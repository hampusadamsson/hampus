#include <stdlib.h>
#include <stdio.h>

int main(){
  int array[5] = {1,2,3,4,5};

  printf("arraynotation - %d\n", array[3]);
  printf("pekararetmitik - %d\n", *(array+3));

		return 0;

}
