defmodule IRCd.Config do
  use ExConfig.Object
  defproperty port, default: 6667
  defproperty name, default: "irc.ircd."
  defproperty operators, default: []
end

