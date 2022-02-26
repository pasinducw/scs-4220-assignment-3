import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.lang.Math
import java.util.concurrent.atomic.AtomicLong

val THREAD_COUNT = 4

@volatile
var primes = new Array[Long](1)

@volatile
var pendingCandidates = new Array[Long](THREAD_COUNT) // candidates that are currently being tested for primality

@volatile
var primesCount = 0 // number of primes found so far

val nextCandidate = new AtomicLong(0) // next candidate to be tested for a prime

val lock = ReentrantLock()

var N = 0 // total number of primes to be computed



object ConcurrentApplication {
  def main(args: Array[String]) = {
    assert(args.length == 1, "must have a single argument, denoting N")
    val t0 = java.lang.System.currentTimeMillis()
    
    N = args(0).toInt
    
    primes = new Array[Long](N)
    
    primes(0) = 2
    primesCount = 1
    nextCandidate.set(3)
    
    val threads = new Array[ComputeThread](THREAD_COUNT)
    for(x <- 0 to (THREAD_COUNT-1)) {
      threads(x) = new ComputeThread(x)
      threads(x).start()
    }

    for(x <- 0 to (THREAD_COUNT-1)) {
      threads(x).join()
    }
    
    val tN = java.lang.System.currentTimeMillis()
    println(primes(N-1))
    println("Time taken: " + (tN - t0))
  }
}

class ComputeThread(val threadId: Int) extends Thread {
  
  val localPrimes = new Array[Long](N)
  var localPrimesCount = 0
  override def run() = {

    while(primesCount < N){
      val candidate = nextCandidate.getAndIncrement()
      pendingCandidates(threadId) = candidate
      var currentProgress = getCurrentProgress()

      while(currentProgress * currentProgress < candidate) {
        // spin
        currentProgress = getCurrentProgress()
      }

      // fill in local primes
      fillInLocalPrimes()

      // compute if the candidate is prime
      val isCandidatePrime = checkPrime(candidate)
      
      // publish the result to global state
      if(isCandidatePrime) publishResult(candidate)
    }
  }

  def getCurrentProgress(): Long = {
    // returns the MIN(pendingCandidates) - 1
    var result = pendingCandidates(0)
    for(i <- 1 to pendingCandidates.length-1) if(pendingCandidates(i) < result) {
      result = pendingCandidates(i)
    }
    return Math.max(result-1, 2)
  }

  def fillInLocalPrimes() = {
    val currentProgress = getCurrentProgress()
    while(localPrimesCount < primesCount && primes(localPrimesCount) <= currentProgress) {
      localPrimes(localPrimesCount) = primes(localPrimesCount)
      localPrimesCount += 1
    }
  }

  def checkPrime(next:Long): Boolean = {
    // test if next is prime
    // invariant: next is coprime with localPrimes[0..i) && p = localPrimes(i)
    var i= 0; var p = localPrimes(i)
    while(i < localPrimesCount && p*p <= next && next % p != 0) { i+= 1; p = localPrimes(i) }
    if(p*p > next || i >= localPrimesCount) { // next is prime
      return true
    }

    return false
  }

  def publishResult(candidate:Long) = {
    lock.lock()
    try {
      if(primesCount < N){
        var index = primesCount
        while((index-1) > 0 && primes(index-1) > candidate){
          primes(index) = primes(index-1)
          index -= 1
        }
        
        primes(index) = candidate
        primesCount += 1
      }
    } finally {
      lock.unlock()
    }
  }
}
