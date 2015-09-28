PROJECT = veritrans
DEPS = ibrowse jsx
REBAR=$(shell which rebar || echo ./rebar)

include erlang.mk

dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.1.1
dep_jsx = git https://github.com/talentdeficit/jsx v1.4.2