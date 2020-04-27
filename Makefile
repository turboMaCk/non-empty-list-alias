test:
	elm-verify-examples && elm-test

format:
	elm-format --yes src

.PHONY: test format
