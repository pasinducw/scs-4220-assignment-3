# Sieve of Eratosthenes Concurrent Implementation

## How to execute the application?
- Run `scala3 -J-Xmx16g ConcurrentApplication.scala [N]` where _N_ should point to the number of primes needed 

## How to benchmark?
- Run `./benchmark.sh`
- This will output a CSV to STDOUT, with the running times of the concurrent and sequential application under different, predefined N values

### Pre-calculated benchmark results
- Refer _results.csv_ file
