#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//#include "db.h"

typedef struct node{
  char *key;
  char *value;
  struct node *left;
  struct node *right;
  //struct node *next;
} *Node;


void readline(char *dest, int n, FILE *source){
  fgets(dest, n, source);
  int len = strlen(dest);
  if(dest[len-1] == '\n')
    dest[len-1] = '\0';
}

void printDb(Node list){       // Print database
  Node cursor = list;
  if(cursor!=NULL) {
    puts(cursor->key);
    puts(cursor->value);
    printDb(cursor->left);
    printDb(cursor->right);
   }
  return;
}


Node updateDb(Node list, char buffer[128]){       // update database
      // Update
      printf("Enter key: ");
      readline(buffer, 128, stdin);
      puts("Searching database...\n");
      int found = 0;
      Node cursor = list;
      while(!found && cursor != NULL){
        if(strcmp(buffer, cursor->key) == 0){
          puts("Matching entry found:");
          printf("key: %s\nvalue: %s\n\n", cursor->key, cursor->value);
          found = 1;
        }else if(strcmp(buffer, cursor->key) < 0) {   
	  cursor = cursor->left;
	}else if(strcmp(buffer, cursor->key) > 0) {
	  cursor = cursor->right;
	
	}else {}
      }
      if(!found){
        printf("Could not find an entry matching key \"%s\"!\n", buffer);
      }else{
        printf("Enter new value: ");
        readline(buffer, 128, stdin);
        free(cursor->value);
        cursor->value = malloc(strlen(buffer) + 1);
        strcpy(cursor->value, buffer);
        puts("Value inserted successfully!");
      }
      return list;
}


Node insertDb(Node list, char buffer[128],char buffer2[128], int val){       // insert database
  //insert
       if (val == 1){
         printf("Enter key: ");
         buffer = malloc(strlen(buffer) + 1);      
         readline(buffer, 128, stdin);
         puts("Searching database for duplicate keys...");
       }
       int found = 0;
       Node cursor = list;
       
       //if(!found){ // Insert new node to the front of the list
       //puts("Key is unique!\n");
       Node newNode = malloc(sizeof(struct node));
       newNode->key = malloc(strlen(buffer) + 1);
       strcpy(newNode->key, buffer);
       if (val == 1){
         printf("Enter value: ");
         buffer2 = malloc(strlen(buffer2) + 1);  
         readline(buffer2, 128, stdin);
       }
       newNode->value = malloc(strlen(buffer2) + 1);
       strcpy(newNode->value, buffer2);
       newNode->left = NULL;
       newNode->right = NULL;
       //cursor = newNode;
       if (val == 1){      
         puts("");
         puts("Entry inserted successfully:");
         printf("key: %s\nvalue: %s\n", newNode->key, newNode->value);
       }
       
       while(!found && cursor != NULL) {
         if(strcmp(buffer, cursor->key) == 0) {
           printf("key \"%s\" already exists!\n", cursor->key);
           found=1;
         }else if(strcmp(buffer, cursor->key) <= 0) {   
           if (cursor->left==NULL){
             cursor->left=newNode;
             cursor=newNode->left;
           }else{
             cursor = cursor->left;
           }
         }else if(strcmp(buffer, cursor->key) > 0) {
           if (cursor->right==NULL){
             cursor->right=newNode;
             cursor=newNode->right;
           }else{
             cursor = cursor->right;
           }   
         }
       }
       if (list==NULL){
         list=newNode;   
       }
       return list;
}



Node readDatabase(char *filename){          // Read the input file
  printf ("Loading database \"%s\"...\n\n", filename);
  FILE *database = fopen(filename, "r");
  char buffer1[128];
  char buffer2[128]; 
  Node list = NULL;
  
  while(!(feof(database))){
    readline(buffer1, 128, database);
    readline(buffer2, 128, database);

    list = insertDb(list, buffer1, buffer2, 0);
   }
  return list;
}


Node deleteDb(Node list, char buffer[128]){       // delete  database
  printf("Enter key: ");
  readline(buffer, 128, stdin);
  puts("Searching database...\n");
  int found = 0;
  Node cursor = list;
  
  while(!found && cursor != NULL) {
    if(strcmp(buffer, cursor->key) == 0) {
      
      found = 1;
      printf("Deleted the following entry:\nkey: %s\nvalue: %s\n", cursor->key,          cursor->value);
      
    }else if(strcmp(buffer, cursor->key) <= 0) {   
      
      cursor = cursor->left;
    }else if(strcmp(buffer, cursor->key) > 0) {
      
      cursor = cursor->right;
    }
  }	
  
  if(found!=1){
    printf("Could not find an entry matching key \"%s\"!\n", buffer);
  }else{
    
    if(cursor->right != NULL && cursor->left != NULL){
      //cursor = hittad node!
      //vill placera 
      
      
    }else if(cursor->left != NULL){
      cursor=cursor->left; 
    }else if(cursor->right != NULL){
      cursor=cursor->right;
    }else{
      //inga löv
    }
  }
  return list;
}

Node queryDb(Node list, char buffer[128]) {         // Query
  printf("Enter key: ");
  readline(buffer, 128, stdin);
  puts("Searching database...\n");
  int found = 0;
  Node cursor = list;
  while(!found && cursor != NULL) {
    if(strcmp(buffer, cursor->key) == 0){
      puts("Found entry:");
      printf("key: %s\nvalue: %s\n", cursor->key, cursor->value); 
      found = 1;
    }else{
      if(strcmp(buffer, cursor->key) <= 0) {   
        cursor = cursor->left;
      }else if(strcmp(buffer, cursor->key) > 0) {
        cursor = cursor->right;
      }else{
      }
    }
  }
  if(!found){
    printf("Could not find an entry matching key \"%s\"!\n", buffer);
  }
  return list;
}
