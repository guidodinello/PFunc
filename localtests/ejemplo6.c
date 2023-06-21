#include <stdio.h>
int _foo(int _x,int _y){
int _let0(int _x){
return (_x); };
int _let1(int _y){
return ((_y + 1)); };
return ((_let0(_y) + _let1((_x + _y)))); };
int _bar(int _x){
int _let1(int _x){
int _let0(int _x){
return (_x); };
return (_let0((_x + 1))); };
return (_let1((_x + 1))); };
int main() {
int _let0(int _x){
return ((_foo(_x,4) + _bar(_x))); };
printf("%d\n",_let0(2)); }
