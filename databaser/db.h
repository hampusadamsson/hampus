#ifndef _DB_H
#define _DB_H


typedef struct node *Node;

Node readDatabase(char *filename); //read a database - a complete list of keys and corresponding values.
void printDb(Node list); //print the current database - value:key
Node updateDb(Node list, char buffer[128]);//update an existing value:key
Node insertDb(Node list, char buffer[128]);//insert new value:key
Node deleteDb(Node list, char buffer[128]);//delete existing value:key
Node queryDb(Node list, char buffer[128]);//print the value of a given key

#endif
