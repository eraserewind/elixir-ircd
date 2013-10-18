defmodule Ircd.Mixfile do
  use Mix.Project

  def project do
    [ app: :ircd,
      version: "0.0.1",
      elixir: "~> 0.10.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      registered: [ :ircd ],
      applications: [ :exlager ],
      mod: { IRCd, [] }
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :lager, github: "basho/lager", override: true },
      { :exlager, ">= 0", github: "khia/exlager" },
      { :exconfig, ">= 0", github: "yrashk/exconfig" },
      { :uuid, ">= 0", github: "avtobiff/erlang-uuid" },
      { :amnesia, "~> 0.1.0", github: "meh/amnesia" },
      { :'elixir-datetime', ">= 0", github: "alco/elixir-datetime" },
    ]
  end
end
