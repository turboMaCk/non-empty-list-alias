.PHONY: test
test:
	elm-verify-examples && elm-test
	elm-format --validate src

.PHONY: format
format:
	elm-format --yes src

