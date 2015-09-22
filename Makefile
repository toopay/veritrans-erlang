PROJECT = veritrans
DEPS = ibrowse
REBAR=$(shell which rebar || echo ./rebar)

include erlang.mk

dep_ibrowse = git https://github.com/cmullaparthi/ibrowse v4.1.1