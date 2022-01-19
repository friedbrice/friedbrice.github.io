.PHONY: list
list: ## Show available targets
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: reset
reset: clean ## Remove project build artifacts and 'bundle' and 'gem' caches
	rm -rf .bundle
	rm -rf .gems
	rm -rf .sass-cache
	rm -rf Gemfile.lock

.PHONY: clean
clean: ## Remove project build artifacts
	rm -rf _site

.PHONY: init
init:
	which ruby || (echo "'ruby' required but not found." && exit 1)
	which gem || (echo "'gem' required by not found." && exit 1)
	which bundle || (echo "'bundle' required by not found." && exit 1)
	bundle config set --local path '.gems'

.PHONY: setup
setup: init update ## Setup Development Environment

.PHONY: update
update: init ## Keep 'jekyll' and external libraries up to date
	bundle update
	bundle install

.PHONY: build
build: init ## Build site locally
	bundle exec jekyll build

.PHONY: watch
watch: init ## Run local development server
	bundle exec jekyll serve

.PHONY: watch-drafts
watch-drafts: init ## Run local development server with drafts
	bundle exec jekyll serve --drafts
