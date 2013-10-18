defmodule IRCd do
  use Application.Behaviour
  require Lager

  def start(_type, _) do
    Amnesia.start
    IRCd.Database.create!
    IRCd.Supervisor.start_link()
  end

  def stop(_state) do
    Lager.info "Stopping app"
    Amnesia.stop
    :ranch.stop_listener(:ircd)
  end

  #def config, do: IRCd.Config.file("ircd.exs")
  def name, do: "irc.ircd"
  def network_name, do: "exIRC"
  def port, do: 6667

  def operators do
    [
      { "pute", "42ftw" },
      { "href", "roxxxor" },
    ]
  end

end
