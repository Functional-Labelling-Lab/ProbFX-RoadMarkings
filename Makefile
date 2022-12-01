.PHONY: clean build run

cpp_dir = backend
bold_magenta = \033[0;35m
reset_col = \033[0m

all: build

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
