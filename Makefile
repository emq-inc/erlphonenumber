.PHONY: test compile shell

compile:
	rebar3 compile

shell:
	rebar3 shell

test:
	rebar3 ct
