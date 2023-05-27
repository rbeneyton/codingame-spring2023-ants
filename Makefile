.PHONY: all, clean

# 		| sed 's@log.*@@' \

all:
	cat src/main.rs \
		| sed 's@^\s*@@' \
		| sed 's@\s*//.*@@' \
		> ../SpringChallenge2023.rs
