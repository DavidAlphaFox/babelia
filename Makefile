help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

check: ## Run tests
	@echo "\n"
	guile -L . check.scm
# @echo "\033[95m\n\nYou may now run 'make lint'.\n\033[0m"

todo: ## Things that should be done
	@grep -nR --color=always --after-context=4 TODO .

xxx: ## Things that require attention
	@grep -nR --color=always --after-context=4 XXX .
