#!/bin/bash

echo "N;Concurrent;Sequential;"

for rep in {1..20}
  do
    for limit in 10 50 100 500 1000 5000 10000 50000 100000 500000 1000000 5000000 10000000 50000000 100000000
      do
        concurrent=$(scala3 -J-Xmx32g ConcurrentApplication.scala ${limit} | grep Time | grep -Eo '[0-9][0-9]*')
        sequential=$(scala3 -J-Xmx32g SequentialApplication.scala ${limit} | grep Time | grep -Eo '[0-9][0-9]*')
        echo "$limit,$concurrent,$sequential"
    done
done
