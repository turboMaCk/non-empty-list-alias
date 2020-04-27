.PHONY: test
test:
	elm-verify-examples && elm-test

.PHONY: format
format:
	elm-format --yes src

