#lang imp/debug/final-env

input n;
fact := 1 ;
while 0 < n do
begin
  fact := fact * n ;
  n := n - 1 ;
end
