defmodule IRCd.Channel do

  require Lager
  require Amnesia

  use GenServer.Behaviour
  use IRCd.Database

  alias IRCd.Protocol, as: P

  # Users: { pid, id, nick, modes }
  defrecord State, uuid: nil, channel: nil, users: [], topic: "exIRCd ROCKS", topic_at: nil, topic_by: nil

  def start_link(ref), do: :gen_server.start_link({ :global, ref }, __MODULE__, [], [])

  def init([]) do
    state = State.new
    state = state.uuid(:uuid.to_string(:uuid.uuid1()))
    debug state, "New channel!"
    { :ok, state }
  end

  # Utility to call server without shitload of args
  defp call_server(action, args, state) do
    :gen_server.call({ :global, :server }, { action, self, state.uuid, args })
  end
  defp cast_server(action, args, state) do
    :gen_server.cast({ :global, :server }, { action, self, state.uuid, args })
  end

  defp make_user_prefix(modes) do
    prefix = if Enum.member?(modes, "v"), do: "+"
    prefix = if Enum.member?(modes, "o"), do: "@"
    unless prefix, do: prefix = ""
    prefix
  end

  # NAMES command
  def c_names(target, state) do
    # TODO: Select state.users to remove target !!
    user_with_prefix = Enum.map(state.users, fn({ _, _, nick, modes}) ->
      prefix = make_user_prefix(modes)
      prefix<>nick
    end)
    debug "user_with_prefixes " <> inspect(user_with_prefix)
    nicks = Enum.join(user_with_prefix, " ")
    [
      ":"<>IRCd.name<>" 353 "<>target<>" = " <>state.channel<> " :"<>nicks,
      ":"<>IRCd.name<>" 366 "<>target<>" "<> state.channel<> " :End of /NAMES list."
    ]
  end

  # WHO
  def c_who(target, state) do
    users = Enum.map(state.users, fn({ u_pid, u_id, u_nick, u_modes }) ->
      prefix = make_user_prefix(u_modes)
      u_user = :gen_server.call(u_pid, { :get_user })
      u_host = :gen_server.call(u_pid, { :get_host })
      u_realn = :gen_server.call(u_pid, { :get_realname })
      info = [target, state.channel, u_user, u_host, IRCd.name, u_nick, "H"<>prefix, ":0", u_realn]
      ":"<>IRCd.name<>" 352 "<>Enum.join(info, " ")
    end)
    end_s = ":"<>IRCd.name<>" 315 "<>target<>" "<>state.channel<>" :End of /WHO list."
    List.flatten(:lists.append([users], [end_s]))
  end

  # TOPIC
  def c_topic(target, state) do
    repls = [
      ":"<>IRCd.name<>" 332 "<>target<>" "<>state.channel<>" :"<>state.topic,
      ":"<>IRCd.name<>" 333 "<>target<>" "<>state.channel<>" server!server@server 954700962"
    ]
    repls
  end

  # Channel creation
  def handle_call({ :create, channel }, _from, state) do
    state = state.channel(channel)
    { :reply, state.uuid, state }
  end

  def handle_call({ :get_users }, _from, state), do: { :reply, state.users, state }

  def handle_call(req, _from, state ) do
    debug state, "unhandled call: " <> inspect(req)
    { :reply, :ok, state }
  end

  # A user join the channel
  def handle_cast({ :join, id }, state) do
    debug state, "user joined " <> inspect(id)
    { :ok, { u_pid, _, u_name, _ } } = :gen_server.call({ :global, :server }, { :get_user, id })
    umodes = if Enum.empty?(state.users) do
      ["o"]
    else
      []
    end
    state = state.users([ { u_pid, id, u_name, umodes } | state.users])
    debug "my state " <> inspect(state)
    # BROADCAST JOIN !
    # 1 get user mask
    mask = :gen_server.call(u_pid, { :get_mask })
    debug "user mask is " <> inspect(mask)
    Enum.map(state.users, fn({ o_u_pid, _, _, _ }) ->
      :gen_server.cast(o_u_pid, { :send, ":"<>mask<>" JOIN "<>state.channel })
    end)
    :gen_server.cast(u_pid, { :send, c_topic(u_name, state) })
    :gen_server.cast(u_pid, { :send, c_names(u_name, state) })
    { :noreply, state }
  end

  def unregister_user(u_id, state) do
    state = state.users(List.keydelete(state.users, u_id, 1))
  end

  def handle_cast({ :unregister_user, u_id}, state) do
    debug state, "Unregistering u_id "<>inspect(u_id)
    state = unregister_user(u_id, state)
    { :noreply, state }
  end

  def handle_cast({:privmsg, u_id, text }, state) do
    debug state, "User "<>inspect(u_id)<>" says "<>inspect(text)
    { :ok, { u_pid, _, u_nick, _ } } = :gen_server.call({ :global, :server }, { :get_user, u_id })
    u_mask = :gen_server.call(u_pid, { :get_mask })
    msg = ":"<>u_mask<>" PRIVMSG "<>state.channel<>" :"<>text
    Enum.each(state.users, fn({o_u_pid, o_u_id, _, _ }) ->
      if o_u_id != u_id, do: :gen_server.cast(o_u_pid, { :send, msg })
    end)
    { :noreply, state }
  end

  def handle_cast({:part, u_id, reason }, state) do
    debug state, "User "<>inspect(u_id)<>" leave "<>inspect(reason)
    { :ok, { u_pid, _, u_nick, _ } } = :gen_server.call({ :global, :server }, { :get_user, u_id })
    u_mask = :gen_server.call(u_pid, { :get_mask })
    msg = ":"<>u_mask<>" PART "<>state.channel<>" :"<>reason
    Enum.each(state.users, fn({o_u_pid, o_u_id, _, _ }) ->
      :gen_server.cast(o_u_pid, { :send, msg })
    end)
    state = state.users(List.keydelete(state.users, u_pid, 0))
    { :noreply, state }
  end

  def handle_cast({ :who, channel, id }, state) do
    debug state, "user asked who " <> inspect(id)
    { :ok, { u_pid, _, u_nick, _ } } = :gen_server.call({ :global, :server }, { :get_user, id })
    :gen_server.cast(u_pid, { :send, c_who(u_nick, state) })
    { :noreply, state }
  end

  def handle_cast(req, state) do
    debug state, "unhandled cast: " <> inspect(req)
    { :noreply, state }
  end

  def handle_info(info, state) do
    debug state, "unhandled info: " <> inspect(info)
    { :noreply, state }
  end

  # ---- Log
  defp debug(state, string) do
    str = "[UUID:" <> inspect(state.uuid) <> "] " <> string
    debug(str)
  end

  defp debug(string), do: Lager.info inspect(self) <> "[IRCd.Channel] " <> string

end
