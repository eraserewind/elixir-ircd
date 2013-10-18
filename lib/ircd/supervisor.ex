defmodule IRCd.Supervisor do
  require Lager
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, [])
  end

  # TODO: Move listening sockets to IRCd.Listener,
  # and spawn a global IRCd.Server
  def init([]) do
    Lager.info inspect(self) <> " Supervisor: init"
    tree = [
      supervisor(IRCd.Server, [ ])
    ]
    supervise(tree, strategy: :one_for_one)
  end

end
