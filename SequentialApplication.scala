
object SequentialApplication {
  def main(args: Array[String]) = {
    assert(args.length == 1, "must have a single argument, denoting N")
    val t0 = java.lang.System.currentTimeMillis()

    val N = args(0).toInt
    val primes = new Array[Long](N)
    
    primes(0) = 2

    var nextSlot = 1 // next free slot in primes
    var next:Long = 3 // next candidate prime to consider

    while(nextSlot < N) {
      // test if next is prime
      // invariant: next is coprime with primes[0..i) && p = primes(i)
      var i= 0; var p = primes(i)
      while(p*p <= next && next % p != 0) { i+= 1; p = primes(i) }
      if(p*p > next) { // next is primes
        primes(nextSlot) = next; nextSlot += 1
      }
      next += 2
    }

    val tN = java.lang.System.currentTimeMillis()
    println(primes(N-1))
    println("Time taken: " + (tN - t0))
  }
}
