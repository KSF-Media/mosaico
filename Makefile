SHELL := bash

# Install git hooks
hooks:
	# ln -sf ./pre-commit.sh .git/hooks/pre-commit
	ln -sf ./pre-push.sh .git/hooks/pre-push