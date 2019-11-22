help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

check: ## Run tests
	@echo "\n"
	guile -L . check.scm

todo: ## Things that should be done
	@grep -nR --color=always --after-context=4 TODO .

xxx: ## Things that require attention
	@grep -nR --color=always --after-context=4 XXX .

bug-guix: bug-guix.tar.gz
	tar xvf bug-guix.tar.gz
	./babelia.scm index bug-guix/

benchmarks: bug-guix ## Wanna be benchmarks
	bash benchmarks.sh > benchmarks.org
