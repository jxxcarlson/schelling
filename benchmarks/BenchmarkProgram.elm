import Benchmark.Runner exposing (BenchmarkProgram, program)
import SchellingBenchmarks exposing(suite)

main : BenchmarkProgram
main =
    program suite
