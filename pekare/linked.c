#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node{
  int *key;
  struct node *next;
} *Node;


Node newnode(int *pekare, Node list){
  Node nynode = malloc(sizeof(struct node));
       nynode->key = malloc(sizeof(struct node));
       nynode->next = malloc(sizeof(struct node));
  Node cursor = list;
  int value = *pekare;

  if(list->key==NULL){
    *(nynode->key) = value;
    (nynode->next) = NULL;
 list=nynode;

   }else{ 
       while(cursor->next != NULL) { 
 	cursor=cursor->next; 
       } 

       *(nynode->key) = value; 
       (nynode->next) = NULL; 
 cursor->next = nynode;   
 
  
 }
return list;
} 
 
void printall(Node list){
  Node cursor = list;     
  while(cursor->next != NULL) { 
 	cursor=cursor->next; 
	printf("%d\n",*(cursor->key));
  }return;
}

int main(){
  Node list=malloc(sizeof(struct node));
  
  for (int i=-1;i<8;++i){
    int *pekare = &i;
    list = newnode(pekare, list);
  }

printall(list);

 /* int x = 45; */
 /* int pekare* = &x; */
 /* newnode(pekare); */

 /* newnode(int pekare*) */
 /* print pekare*; */
  return 0;
}


