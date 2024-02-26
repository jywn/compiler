#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "varlist.h"
// TODO: Fill in
VarNode* make_varlist(char *var, int val, VarNode* next) {
    VarNode* curr = (VarNode *)malloc(sizeof(VarNode));
    curr->var = var;
    curr->val = val;
    curr->next = next;
    return curr;
    //;
    //7 + 3 * 4
}

// TODO: Fill in
int lookup_var(VarNode *head, char *var) {
    VarNode* curr = head;
    for(curr = head; curr != NULL; curr = curr->next) {
        if(strcmp(curr->var, var) == 0) {
            return curr->val;
        }
    }
    return 0;
}

// TODO: Fill in
void free_varlist(VarNode *head) {
    //전체 free
    VarNode* curr = NULL;
    VarNode* prev = NULL;
    for(curr = head ; curr != NULL; curr = curr->next) {
        if(prev)    free(prev);
        prev = curr;
    }
    if(prev)    free(prev);
    return;
}
