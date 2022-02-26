import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicStampedReference
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicInteger
import java.lang.Math
import java.util.concurrent.atomic.AtomicLong

@volatile
var rollingCandidateStatuses = new Array[Long](1)
@volatile
var primes = new Array[Long](1)
@volatile
var primesCount = 0

val primesNextIndex = new AtomicInteger(0)
val progress = new AtomicLong(0) // number of candidates tested so far [contiguous]
val progress2 = new AtomicLong(0) // number of candidates tested so far [contiguous] (used to synchronize multiple threads)
// val primesCount = new AtomicInteger(0) // number of primes discovered so far [contiguous]
val nextCandidate = new AtomicLong(0) // next candidate to be tested for a prime
var N = 0

object SequentialApplication {
  def main(args: Array[String]) = {
    assert(args.length == 1, "must have a single argument, denoting N")
    val t0 = java.lang.System.currentTimeMillis()
    
    N = args(0).toInt
    
    rollingCandidateStatuses = new Array[Long](2*N)
    primes = new Array[Long](2*N)
    
    primes(0) = 2
    progress.set(2)
    progress2.set(2)
    nextCandidate.set(3)
    primesCount = 1
    
    // do the thing
    // for (x <- 1 to 2) {
    //   val th = new ComputeThread()
    //   th.setName(x.toString())
    //   th.start()
    // }
    val threadCount = 4
    val threads = new Array[ComputeThread](threadCount)
    for(x <- 0 to (threadCount-1)) {
      threads(x) = new ComputeThread()
      threads(x).start()
    }

    for(x <- 0 to (threadCount-1)) {
      threads(x).join()
    }
    
    val tN = java.lang.System.currentTimeMillis()
    println(primes(N-1))
    println("Time taken: " + (tN - t0))
  }
}

class ComputeThread extends Thread {
  val localPrimes = new Array[Long](2*N)
  var localPrimesNextIndex = 0
  override def run() = {

    while(primesCount < N){
      val candidate = nextCandidate.getAndIncrement()
      // printf("QUERY isPrime(%d)\n", candidate)

      // spin while sqrt(candidate) is not yet computed
      var currentProgress = progress.get()
      // printf("\t%d queries verified and available\n", currentProgress)
      while(currentProgress * currentProgress < candidate) {
        // sleep?
        // printf("\t\t\t\t[SPINNING] Need at least progress: %f, Current Progress: %d, currentProgress^2: %d, candidate: %d\n", Math.sqrt(candidate), currentProgress, currentProgress * currentProgress, candidate)
        currentProgress = progress.get()
      }
      // printf("\t\t%d queries verified and available\n", currentProgress)

      // fill in local primes
      fillInLocalPrimes()

      // compute if the candidate is prime
      val isCandidatePrime = checkPrime(candidate)
      // printf("RESPONSE isPrime(%d) = %s\n", candidate, isCandidatePrime.toString())

      // publish the result to global state
      publishResult(candidate, isCandidatePrime)

      // make progress
      makeProgress()
    }

    // printf("DONE\n")
  }

  def fillInLocalPrimes() = {
    val targetIndex = primesNextIndex.get()
    while(localPrimesNextIndex < targetIndex) {
      localPrimes(localPrimesNextIndex) = primes(localPrimesNextIndex)
      localPrimesNextIndex += 1
    }
  }

  def checkPrime(next:Long): Boolean = {
    // test if next is prime
    // invariant: next is coprime with localPrimes[0..i) && p = localPrimes(i)
    var i= 0; var p = localPrimes(i)
    while(i < localPrimesNextIndex && p*p <= next && next % p != 0) { i+= 1; p = localPrimes(i) }
    if(p*p > next || i >= localPrimesNextIndex) { // next is prime
      return true
    }

    return false
  }

  def publishResult(candidate:Long, isPrime: Boolean) = {
    val hashSize = 2 * N
    // spin while the progress has caught up
    while(progress.get() + hashSize < candidate){
      // sleep?
      // printf("\tWAIT hash is at full capacity. Cannot publish result yet\n")
    }

    // publish result to the hash
    val hashIndex: Int = (candidate % hashSize).toInt
    if(isPrime)
      rollingCandidateStatuses(hashIndex) = candidate 
    else 
      rollingCandidateStatuses(hashIndex) = -candidate
  }

  def makeProgress(): Boolean = {
    val hashSize = 2 * N
    while(true){
      val currentProgress = progress.get()
      val nextItem = currentProgress+1
      val nextItemHash: Int = ((nextItem) % hashSize).toInt
      if(rollingCandidateStatuses(nextItemHash) == nextItem || rollingCandidateStatuses(nextItemHash) == -nextItem){
        // printf("\tNEW PROGRESS (%d)\n", nextItem)
        // next item has been computed
        if(rollingCandidateStatuses(nextItemHash) == nextItem){
          // nextItem is a prime
          if(progress.compareAndSet(currentProgress, nextItem)){
            // printf("\t\tUPDATE Primes array (%d)\n", nextItem)
            // while(progress2.get() != currentProgress) {
            //   // sleep?
            //   printf("\tThere seems to be an intermetent task going on. Waiting until it's done. (%d, %d)\n", progress.get(), progress2.get())
            // }
            primes(primesNextIndex.getAndIncrement()) = nextItem
            primesCount += 1
            // progress2.compareAndSet(currentProgress, nextItem)
          } else {
            // printf("\t\tFAILED UPDATE (expectedCurrentProgress = %d, actualCurrentProgress = %d)\n", currentProgress, progress.get())
          }
        } else {
          // nextItem is not a prime
          if(progress.compareAndSet(currentProgress, nextItem)){
            // printf("\t\tUPDATE Progress ONLY (bc %d is not a prime)\n", nextItem)
            // progress2.compareAndSet(currentProgress, nextItem)
          } else {
            // printf("\t\tFAILED UPDATE (expectedCurrentProgress = %d, actualCurrentProgress = %d)\n", currentProgress, progress.get())
          }
        }
      } else {
        // next item has not yet been computed
        // therefore, can't make any progress
        // printf("\tNext contiguous item (%d) has not yet been computed. Next candidate (%d)\n", nextItem, nextCandidate.get())
        return false
      }
    }

    return true
  }
}
