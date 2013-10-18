defmodule IRCd.Server do
  require Lager
  require Amnesia

  use GenServer.Behaviour
  use IRCd.Database

  def start_link() do
    :gen_server.start_link({ :global, :server }, __MODULE__, [], [])
  end

  # users: { pid, id, nick, modes }
  # channels: { pid, id, name, modes }
  defrecord State, users: [], channels: [], opers: []

  def init([]) do
    case :gen_tcp.listen(IRCd.port, [{:packet, :line}, {:reuseaddr, true}]) do
      { :ok, listen_socket } ->
        debug "Started on :"<>inspect(IRCd.port)<>"!"
        spawn(IRCd.Server, :accept_connection, [ listen_socket ])
        { :ok, State.new }
      { :error, reason } ->
        debug "Listen error: " <> reason
        { :stop, "Couldn't listen: " <> reason }
    end
  end

  # Call messages

  def handle_call({ :nick, pid, uuid, [new_nick] }, _from, state) do
    debug "got call:nick --> " <> new_nick
    case List.keyfind(state.users, new_nick, 2) do
      { _, _, _, _ } ->
        debug "OHFUCKINGHELL NICK ALREADY TAKEN!!!!!!!!!!!!!!"
        { :reply, :fail, state }
      _ ->
        state = state.users([{pid,uuid,new_nick,[]}|List.keydelete(state.users, pid, 0)])
        debug "New state " <> inspect(state)
        { :reply, :ok, state }
    end
  end

  def handle_call({ :oper, pid, uuid }, _from, state) do
    debug "got call:oper --> " <>inspect(uuid)
    case List.keyfind(state.opers, uuid, 1) do
      { _, _, _, _ } -> { :reply, :ok, state }
      _ ->
        state = state.opers([{ pid, uuid } | state.opers ])
        debug "oper registered!"
        { :reply, :ok, state }
    end
  end

  # Find a user/channel

  def handle_call({ :get_user_pid, key }, _from, state) do # when is_bitstring(key) do
    debug "get_user_pid" <> inspect(key)
    case List.keyfind(state.users, key, 1) do
      { pid, _, _, _ } -> { :reply, { :ok, pid }, state }
      _ -> { :reply, :fail, state }
    end
  end

  # get user by uuid
  def handle_call({ :get_user, key }, _from, state), do: get_user(key, 1, state)
  def handle_call({ :get_user, key, key_pos }, _from, state), do: get_user(key, key_pos, state)
  #  debug "get_user_pid" <> inspect(key)
  #  case List.keyfind(state.users, key, key_pos) do
    #    { pid, id, name, modes } -> { :reply, { :ok, { pid, id, name, modes } }, state }
    #  _ -> { :reply, :fail, state }
    #end
    #end

  def get_user(key, position, state) do
    case List.keyfind(state.users, key, position) do
      { pid, id, name, modes } -> { :reply, { :ok, { pid, id, name, modes } }, state }
      _ -> { :reply, :fail, state }
    end
  end

  def handle_call({ :get_channel_pid, key }, _from, state) do # when is_bitstring(key) do
    debug "get_channel_pid" <> inspect(key)
    case List.keyfind(state.channels, key, 1) do
      { pid, _, _, _ } -> { :reply, { :ok, pid }, state }
      _ -> { :reply, :fail, state }
    end
  end

  # get channel by uuid
  def handle_call({ :get_channel, key }, _from, state) do # when is_bitstring(key) do
    debug "get_channel_pid" <> inspect(key)
    case List.keyfind(state.channels, key, 1) do
      { pid, id, name, modes } -> { :reply, { :ok, { pid, id, name, modes } }, state }
      _ -> { :reply, :fail, state }
    end
  end
  # and by name
  def handle_call({ :get_channel_by_name, key }, _from, state) do # when is_bitstring(key) do
    debug "get_channel_by_name" <> inspect(key)
    case List.keyfind(state.channels, key, 2) do
      { pid, id, name, modes } -> { :reply, { :ok, { pid, id, name, modes } }, state }
      _ -> { :reply, :fail, state }
    end
  end

  def handle_call({ action, pid, uuid, [args] }, _from, state) do
    debug "unhandled normalized call from "<>inspect(pid)<>": " <> inspect(action) <> " " <> inspect(args)
    { :reply, :fail, state }
  end

  def handle_call({ :printstate }, _from, state) do
    debug "asked to print state"
    debug inspect(state)
    { :reply, :ok, state }
  end

  def handle_call(request, _from, state) do
    debug "handle_call: unexepected: " <> inspect(request)
  end

  # Cast messages

  # An user is joining a channel
  def handle_cast({ :join, pid, id, [channel] }, state) do
    debug "join! o/ " <> inspect(channel) <> inspect(pid)
    { _, id, nick, _ } = List.keyfind(state.users, pid, 0)
    { c_pid, c_id, state } = get_or_create_channel(channel, state)
    debug "join found the channel pid! " <> inspect(c_pid)
    :gen_server.cast(c_pid, { :join, id })
    :gen_server.cast(pid, { :join, c_id })
    { :noreply, state }
  end

  # channel { pid, uuid, name }
  def get_or_create_channel(channel, state) do
    case List.keyfind(state.channels, channel, 2) do
      { c_pid, c_id, _, modes } -> { c_pid, c_id, state }
      _ ->
        ### TODO: Create channel
        debug "channel "<>inspect(channel)<> " not found, should create it"
        { :ok, c_pid } = IRCd.Channel.start_link(:erlang.make_ref())
        c_id = :gen_server.call(c_pid, { :create, channel })
        debug "channel uuid is " <> inspect(c_id)
        state = state.channels([{ c_pid, c_id, channel, ["n", "t"] }|state.channels])
    end
    debug "my state " <> inspect(state)
    { c_pid, c_id, state }
  end

  def handle_cast({ :quit, id, reason }, state) do
    { state, u_pid } = process_quit(id, reason, state)
    :gen_server.cast(u_pid, { :shutdown })
    { :noreply, state }
  end

  def process_quit(id, reason, state) do
    debug "someone is leaving! " <> inspect(id) <> " - "<>inspect(reason)
    { u_pid, _, _, _ } = List.keyfind(state.users, id, 1)
    u_mask = :gen_server.call(u_pid, { :get_mask })
    o_users = Enum.map(state.channels, fn({ c_pid, c_id, _, _}) ->
      chan_users = :gen_server.call(c_pid, { :get_users })
      if List.keyfind(chan_users, id, 1) do
        :gen_server.cast(c_pid, { :unregister_user, id })
        Enum.map(chan_users, fn({ c_pid, _, _, _}) -> unless c_pid == u_pid, do: c_pid end)
      else
        []
      end
    end)
    msg = ":"<>u_mask<>" QUIT "<>" :"<>reason
    debug "users to be notified of quit"<>inspect(o_users)
    Enum.each(List.flatten(o_users), fn(pid) ->
      :gen_server.cast(pid, { :send, msg })
    end)

    state = state.users(List.keydelete(state.users, id, 1))
    { state, u_pid }
  end

  def handle_cast({ :kill, oper_id, nick, reason }, state) do
    { u_pid, _, u_nick, _ } = List.keyfind(state.users, oper_id, 1)
    u_mask = :gen_server.call(u_pid, { :get_mask })
    case List.keyfind(state.users, nick, 2) do
      { k_pid, k_id, _, _ } ->
        :gen_server.cast(k_pid, { :send, ":"<>u_mask<>" KILL "<>nick<>" :"<>reason })
        quit_reason = "Killed ("<>u_nick<>" ("<>reason<>"))"
        { state, _ } = process_quit(k_id, quit_reason, state)
        :gen_server.cast(k_pid, { :shutdown, :killed, quit_reason })
      _ -> :gen_server.cast(u_pid, { :send_snumeric, "401", nick<>" :No such nick" })
    end
    { :noreply, state }
  end

  # Utility for console
  def handle_cast({ :remove_nick, nick }, state) do
    { _, u_id, _, _ } = List.keyfind(state.users, nick, 2)
    { state, u_pid } = process_quit(u_id, "Removed from server", state)
    quit_reason = "Killed (server (removed from server))"
    :gen_server.cast(u_pid, { :shutdown, :killed, quit_reason })
    { :noreply, state }
  end

  def handle_cast(message, state) do
    debug "handle_cast: unexpected: " <> inspect(message)
    { :noreply, state }
  end

  # Info messages (non call/cast)

  def handle_info(info, state) do
    debug "handle_info: unexpected: " <> inspect(info)
    { :noreply, state }
  end

  # Terminate

  def code_change(_old, state, _extra), do: {:ok, state}

  def terminate(:normal, _state), do: :ok
  def terminate(reason, _state) do
    debug "Server terminated " <> inspect(reason)
    :ok
  end

  # Internal Functions

  def accept_connection(listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      { :ok, socket } ->
        ref = :erlang.make_ref()
        { :ok, pid } = IRCd.User.start_link(ref)
        :gen_tcp.controlling_process(socket, pid)
        :gen_server.cast(pid, { :create, socket })
      { :error, reason } ->
        debug "ERR! Could not accept: " <> inspect(reason)
        #debug reason # FIXME had to make it crash
        exit(reason)
    end
    accept_connection(listen_socket)
  end

  defp debug(string), do: Lager.info inspect(self) <> "[IRCd.Server] " <> string

end
