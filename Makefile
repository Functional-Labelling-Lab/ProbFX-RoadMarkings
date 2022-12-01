.PHONY: clean build run bench

cpp_dir = backend
bold_magenta = \033[0;35m
reset_col = \033[0m
benchmark_prog = benchmark.py

all: build

bench: build
	@printf "$(bold_magenta)>> Running Benchmark Suite <<$(reset_col)"
	@printf "$(bold_magenta)>> python3 version 3.10+   <<$(reset_col)"
	python3 $(benchmark_prog)

run: build
	@printf "$(bold_magenta)>> Running cabal side      <<$(reset_col)"
	cabal run

build:
	@printf "$(bold_magenta)>> Cleaning Haskell Side   <<$(reset_col)\n"
	cabal clean 
	@printf "$(bold_magenta)>> Rebuilding Cabal side   <<$(reset_col)\n"
	@printf "$(bold_magenta)>> Will rebuild CPP side   <<$(reset_col)\n"
	cabal build
	@printf "$(bold_magenta)>> Build Complete          <<$(reset_col)\n"

clean:
	@printf "$(bold_magenta)>> Cleaning Haskell side   <<$(reset_col)\n"
	cabal clean
	@printf "$(bold_magenta)>> Instruct CPP to clean   <<$(reset_col)\n"
	make --directory=$(cpp_dir) clean
	@printf "$(bold_magenta)>> Cleaning complete       <<$(reset_col)\n"
