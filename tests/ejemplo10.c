#include <stdio.h>
int _foo(int _x,int _y){
int _let0(int _x){
return (_x); };
int _let1(int _y){
return (_y); };
return ((_let0(_y) + _let1((_x + _y)))); };
int _bar(int _x){
int _let0(int _x){
return (_x); };
int _let0(int _x){
return (_x); };
return ((_foo(_let0(5),_x)==_foo(_x,_let0(5)))); };
int main() {
printf("%d\n",(_foo(2,4) + ((_bar(5)==1)?3:4))); }
