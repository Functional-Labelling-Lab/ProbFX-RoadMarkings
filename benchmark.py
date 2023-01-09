import subprocess, tempfile, re
from typing import Tuple, Any, Optional

BENCHMARK_SIZE: int = 1
BENCHMARK_BATCH_SIZE: int = 1
BENCHMARK_MSG = f"""
======================================================
ProbFX RoadMarkings Benchmarking Suite.
------------------------------------------------------
This scripts takes random parameters for the target 
road, synthetically generates the road and gets the 
accuracy.

{BENCHMARK_SIZE} benchmarks are run in batches of {BENCHMARK_BATCH_SIZE}.
"""


def start_benchmark(iteration: int) -> Tuple[int, Any, Any]:
    print(f"Starting Benchmark:  {iteration:3}")
    output = tempfile.TemporaryFile()
    return (iteration, output,
            subprocess.Popen(
                [f"cabal run ProbFX-RoadMarkings -- {iteration} | grep \"FINAL ACCURACY: \""],
                stdout=output, stderr=output, shell=True))


def extract_benchmark(output: bytes) -> Optional[float]:
    if (res := re.match("FINAL ACCURACY: *(\d.*)[\n ]*", output.decode("utf-8"))) is not None:
        (acc,) = res.groups()
        return float(acc)
    else:
        print(f"Failed to extract accuracy from {output}")


def end_benchmark(iteration: int, output: Any, process: Any) -> Optional[float]:
    print(f"Waiting on benchmark:{iteration:3}")
    process.wait()
    output.seek(0)
    contents = output.readlines()
    output.close()
    print(f"Completed Benchmark: {iteration:3}")
    return extract_benchmark(contents[-1])


def run_benchmarks(iterations: int) -> float:
    print(BENCHMARK_MSG)
    results: list[float] = []

    for batch in range(0, iterations, BENCHMARK_BATCH_SIZE):
        benchmarks = [start_benchmark(i) for i in range(
            batch, min(batch + BENCHMARK_BATCH_SIZE, iterations))]
        results.extend([res for bench in benchmarks if (res := end_benchmark(*bench)) is not None])

    print("\nList of accuracies:")
    print(' '.join([f"{round(f * 100, 2)}%" for f in results]))

    mean : float = 100 * sum(results) / len(results)
    print(f"Average accuracy: {round(mean,2):3}")
    return mean


if __name__ == "__main__":
    run_benchmarks(BENCHMARK_SIZE)
