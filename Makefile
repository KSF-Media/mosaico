SHELL := bash

all: generate-branch-ci generate-master-ci

.PHONY: generate-branch-ci generate-master-ci

generate-branch-ci:
	npx dhall-to-yaml --omit-empty --file "./ci/ci-pull-request.dhall" --output .github/workflows/previews.yml;

generate-master-ci:
	npx dhall-to-yaml --omit-empty --file "./ci/ci-master.dhall" --output .github/workflows/production.yml

# Install git hooks
hooks:
	# ln -sf ./pre-commit.sh .git/hooks/pre-commit
	ln -sf ./pre-push.sh .git/hooks/pre-push