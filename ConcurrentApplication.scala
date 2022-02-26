import java.lang.Math
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicLongArray

val THREAD_COUNT = 4

val lock = ReentrantLock()

var primes = new AtomicLongArray(1)
val nextCandidate = new AtomicLong(0) // next candidate to be tested for a prime
var pendingCandidates = new AtomicLongArray(THREAD_COUNT)

@volatile
var primesCount = 0 // number of primes found so far
@volatile
var contiguousPrimesCount = 0 // number of contiguous primes found so far

var N = 0 // total number of primes to be computed

object ConcurrentApplication {
  def main(args: Array[String]) = {
    assert(args.length == 1, "must have a single argument, denoting N")
    val t0 = java.lang.System.currentTimeMillis()
    
    N = args(0).toInt
    
    primes = new AtomicLongArray(N)
    
    primes.set(0, 2)
    primesCount = 1
    contiguousPrimesCount = 1
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
    println(primes.get(N-1))
    println("Time taken: " + (tN - t0))
  }
}

class ComputeThread(val threadId: Int) extends Thread {
  
  val localPrimes = new Array[Long](N)
  var localPrimesCount = 0
  override def run() = {

    while(contiguousPrimesCount < N){
      val candidate = nextCandidate.getAndAdd(2)
      pendingCandidates.set(threadId, candidate)
      var currentProgress = getCurrentProgress()

      while(currentProgress * currentProgress < candidate) {
        // spin
        currentProgress = getCurrentProgress()
      }

      // fill in local primes
      fillInLocalPrimes(candidate)

      // compute if the candidate is prime
      val isCandidatePrime = checkPrime(candidate)
      
      // publish the result to global state
      if(isCandidatePrime) publishResult(candidate)
    }
  }

  def getCurrentProgress(): Long = {
    // returns the MIN(pendingCandidates) - 1
    var result = pendingCandidates.get(0)
    for(i <- 1 to THREAD_COUNT-1) if(pendingCandidates.get(i) < result) {
      result = pendingCandidates.get(i)
    }
    return Math.max(result-1, 2)
  }

  def fillInLocalPrimes(candidate: Long) = {
    val currentProgress = getCurrentProgress()
    val currentContiguousPrimesCount = contiguousPrimesCount
    while(localPrimesCount < currentContiguousPrimesCount) {
      localPrimes(localPrimesCount) = primes.get(localPrimesCount)
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
      if(contiguousPrimesCount < N){
        var index = Math.min(primesCount, N)
        while((index-1) > 0 && primes.get(index-1) > candidate){
          if(index < N) primes.set(index, primes.get(index-1)) // if index == N, we discard the computed value
          index -= 1
        }
        
        if(index < N) primes.set(index, candidate) // if index == N, we discard the computed value
        primesCount += 1

        val currentProgress = getCurrentProgress() // all primes up to this point are already in final form
        val currentPrimesCount = Math.min(primesCount, N)
        while(contiguousPrimesCount < currentPrimesCount && primes.get(contiguousPrimesCount) <= currentProgress+1) contiguousPrimesCount += 1
      }
    } finally {
      lock.unlock()
    }
  }
}
