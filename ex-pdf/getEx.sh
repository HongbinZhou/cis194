#!/bin/sh

for i in $( cat lectures.html| grep 'hw\/' | awk -F= '{print $2}' | awk -F\" '{print $2}' ); do wget http://www.seas.upenn.edu/\~cis194/spring13/$i; done

for i in $( cat lectures.html| grep 'lectures\/' | awk -F= '{print $2}' | awk -F\" '{print $2}' ); do wget http://www.seas.upenn.edu/\~cis194/spring13/$i; done
