package kits.eff

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{ State => S, _ }

@S(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 3, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class Benchmarks {

  @Benchmark
  def benchEffJmh(): Unit = {
    val bench = new Bench()

    bench.benchEffCall2()
  }

  @Benchmark
  def benchTransJmh(): Unit = {
    val bench = new Bench()

    bench.benchTransCall()
  }
}
