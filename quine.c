#include<stdio.h>
int main(){
char sym = 10;
char sym1 = 34;
char s1[] = "printf(";
char s2[] = "#include<stdio.h>%cint main(){%cchar sym = 10;%cchar sym1 = 34;%cchar s1[] = %cprintf(%c;%cchar s2[] = %c%s%c;%cchar s3[] = %c%s%c;%c%s%c%s%c%s;%c}%c";
char s3[] = ",sym,sym,sym,sym,sym1,sym1,sym,sym1,s2,sym1,sym,sym1,s3,sym1,sym,s1,sym1,s2,sym1,s3,sym,sym)";
printf("#include<stdio.h>%cint main(){%cchar sym = 10;%cchar sym1 = 34;%cchar s1[] = %cprintf(%c;%cchar s2[] = %c%s%c;%cchar s3[] = %c%s%c;%c%s%c%s%c%s;%c}%c",sym,sym,sym,sym,sym1,sym1,sym,sym1,s2,sym1,sym,sym1,s3,sym1,sym,s1,sym1,s2,sym1,s3,sym,sym);
}
