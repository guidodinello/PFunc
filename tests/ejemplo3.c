#include <stdio.h>
int _foo(int _x,int _b){
return ((_b?(_x * _x):(_x + 2))); };
int main() {
printf("%d\n",((23 + _foo(2,1)) + _foo(3,0))); }
