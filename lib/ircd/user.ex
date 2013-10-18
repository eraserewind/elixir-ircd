defmodule IRCd.User do
  @moduledoc """
  Handles an IRC connection
  """

  require Lager
  require Amnesia

  use GenServer.Behaviour
  use IRCd.Database

  alias IRCd.Protocol, as: P

  defrecord State, uuid: nil, nick: nil, user: nil, realname: nil, host: nil, ip: nil,
                    registered: nil, socket: nil, channels: [], oper: false,
                    connected_at: nil, registered_at: nil, pong_at: nil

  def start_link(ref), do: :gen_server.start_link({ :global, ref }, __MODULE__, [], [])

  defp timer_interval, do: 60000

  def init([]) do
    state = State.new
    state = state.uuid(:uuid.to_string(:uuid.uuid1())).connected_at(Time.now)
    debug state, "New user!"
    :erlang.send_after(timer_interval, self, :timer)
    { :ok, state }
  end

  # ---- Call messages

  def get_mask(state) do
    if state.registered do
      state.nick<>"!"<>state.user<>"@"<>state.host
    else
      "anonymous!unregistered@server"
    end
  end

  def handle_call({ :get_mask }, _from, state) do
    { :reply, get_mask(state), state }
  end

  def handle_call({ :get_user }, _from, state) do
    { :reply, state.user, state }
  end

  def handle_call({ :get_host }, _from, state) do
    { :reply, state.host, state }
  end

  def handle_call({ :get_realname }, _from, state) do
    { :reply, state.realname, state }
  end

  defp get_whois_data(state), do: { state.user, state.host, state.realname, state.oper, state.pong_at, state.registered_at }

  def handle_call({ :get_whois_data }, _from, state), do: { :reply, get_whois_data(state), state }

  def handle_call(request, _from, state) do
    debug state, "handle_call: unexepected: " <> inspect(request)
  end

  # ---- Cast messages

  # Socket creation
  def handle_cast({ :create, socket }, state) do
    { :ok, { ip, port } } = :inet.peername(socket)
    { :ok, ip_string } = String.from_char_list(:inet_parse.ntoa(ip))
    { :ok, host } = case :inet.gethostbyaddr(ip) do
      { :ok, { :hostent, hostname, _, _, _, _ } } -> String.from_char_list(hostname)
      { :error, _ } -> String.from_char_list(ip_string)
    end
    state = state.host(host).ip(ip_string).socket(socket)
    { :noreply, state }
  end

  # Joined a channel
  # TODO: check that the channel does not exists inside the list
  def handle_cast({ :join, id }, state) do
    debug state,"I joined channel " <> inspect(id)
    #{ :ok, c_pid } = :gen_server.call({ :global, :server }, { :get_channel_pid, id })
    { :ok, { c_pid, _, c_name, _ } } = :gen_server.call({ :global, :server }, { :get_channel, id })
    state = state.channels([ { c_pid, id, c_name } | state.channels ])
    # I think we're done here
    debug state, "my state " <> inspect(state)
    { :noreply, state }
  end

  def handle_cast({ :send, data }, state) do
    send(state.socket, data)
    { :noreply, state }
  end

  def handle_cast({ :shutdown, :killed, reason }, state) do
    debug state, "Got kill'd "<>reason
    mask = get_mask(state)
    repl = [
      ":"<>mask<>" QUIT :"<>reason,
      "ERROR :Closing Link: "<>state.host<>" ("<>reason<>")"
    ]
    send(state.socket, repl)
    if state.socket do
      :gen_tcp.close(state.socket)
      state = state.socket(nil)
    end
    { :stop, :normal, state }
  end

  def handle_cast({ :shutdown }, state) do
    debug state, "I've had a good life! goodbye cruel world!"
    if state.socket do
      :gen_tcp.close(state.socket)
      state = state.socket(nil)
    end
    { :stop, :normal, state }
  end

  def handle_cast(message, state) do
    debug state, "handle_cast: unexpected: " <> inspect(message)
    { :noreply, state }
  end

  # ---- Info messages (non call/cast)

  def handle_info({ :tcp, socket, data}, state) do
    debug state, " >>>> " <> inspect(data)
    { :ok, command, args } = P.parse_input(data)
    command = String.downcase(command)
    { :ok, command_list } = String.to_char_list(command)
    command = list_to_atom(command_list)
    debug state, " >>>> command:" <> inspect(command) <> " args: " <> inspect(args)
    state = state.pong_at(Time.now)
    case handle_irc(command, args, state) do
      { :ok, state } -> nil
      { :ok, reply, state } -> send(socket, reply)
      # TODO: error mgmt
    end
    { :noreply, state }
  end

  def handle_info(:timer, state) do
    elapsed = round(Time.elapsed(state.pong_at, :sec))
    debug state, "elapsed time since last pong: "<>inspect(elapsed)
    if elapsed > 60, do: send(state.socket, ":"<>IRCd.name<>" PING " <> state.nick)
    if elapsed > 242 do
      :gen_server.cast({:global, :server}, {:quit, state.uuid, "Ping timeout: "<>inspect(elapsed)<>" seconds"})
    else
      # --
      :erlang.send_after(timer_interval, self, :timer)
    end
    { :noreply, state }
  end

  def handle_info(info, state) do
    debug state, "handle_info: unexpected: " <> inspect(info)
    { :noreply, state }
  end

  # ---- IRC !!!

  # Utility to call server without shitload of args
  defp call_server(action, args, state) do
    :gen_server.call({ :global, :server }, { action, self, state.uuid, args })
  end
  defp cast_server(action, args, state) do
    :gen_server.cast({ :global, :server }, { action, self, state.uuid, args })
  end

  def handle_irc(:nick, [nick], state) do
    debug state, "NICK!!! :) " <> inspect(nick)
    repl_nick = if state.nick do
      state.nick
    else
      nick
    end
    case call_server(:nick, [nick], state) do
      :fail -> { :ok, ":"<>IRCd.name<>" 433 "<>nick<>" :Nickname already in use.", state }
      :ok ->
        state = state.nick(nick)
        { replies, state } = send_register(state)
        { :ok, replies, state }
    end
  end

  defp snumeric(num, text, state) do
    P.format_numeric(IRCd.name, num, state.nick, text)
  end
  defp unumeric(num, text, state) do
    P.format_numeric(state.nick, num, state.nick, text)
  end
  defp snotice(notice, state), do: "NOTICE "<>state.nick<>" :"<>notice

  def handle_irc(:user, [user, mode, _, realname], state) do
    debug state, "USER! u: " <> user <> " realname: " <> realname
    state = state.user(user).realname(realname)
    { replies, state } = send_register(state)
    { :ok, replies, state }
  end

  def send_register(state) do
    if state.registered do
      debug state, "Already registered"
      { [], state }
    else
      if state.nick && state.user && state.realname do
        debug state, "Registering user!"
        replies = [
          snumeric(:welcome, "Welcome to the " <> IRCd.network_name <> "Internet Relay Chat Network " <> state.nick, state),
          snumeric(:yourhost, "Your host is "<>IRCd.name<>", running exIRCd-0.0.00042", state),
          snumeric(:created, "This server was created Mon Aug 4 2042 at 24:42:42 MOON", state),
          snumeric(:myinfo, IRCd.name<>" exIRCd-0.0.0.42 DQRSZaghilopswz CFILPQSbcefgijklmnopqrstvz bkloveqjfI", state),
          snumeric(:bounce, "CHANTYPES=# PREFIX=(ov)@+ NETWORK=exIRCd :are supported by this server", state),
          snumeric("251", "There are 42 users and 42 invisible on 42 servers", state),
          snumeric("254", "42 :channels formed", state),
          snumeric("255", ":I have 42 clients and 42 servers", state),
          snumeric("265", "6 8 :Current local users 6, max 8", state),
          snumeric("266", "6 8 :Current global users 6, max 8", state),
          snumeric("250", ":Highest connection count: 8 (8 clients) (11 connections received)", state),
          snotice("Your connection identifier is "<>inspect(state.uuid), state),
          #unumeric("MODE", ":+iox", state),
        ]
        { :ok, motd, state } = handle_irc(:motd, state)
        state = state.registered(true).registered_at(Time.now).pong_at(Time.now)
        { [replies | motd ], state }
      else
        debug state, "Registration sequence not complete yet"
        { [], state }
      end
    end
  end

  def handle_irc(:motd, state), do: handle_irc(:motd, [], state)

  def handle_irc(:motd, args, state) do
    replies = [
      snumeric("375", ":- "<>IRCd.name<>" Message of the Day -", state),
      snumeric("372", ":- The developer is lazy and hasn't implemented MOTDs yet.", state),
      snumeric("376", ":- ", state),
    ]
    { :ok, replies, state }
  end

  def handle_irc(:whois, [nick], state) do
    debug state, "WHOIS "<>nick
    case :gen_server.call({:global,:server}, { :get_user, nick, 2 }) do
      { :ok, { u_pid, u_id, u_nick, u_modes } } ->
        { user, host, realname, oper, pong_at, co_at } = if u_pid == self do
          get_whois_data(state)
        else
          :gen_server.call(u_pid, { :get_whois_data })
        end
        debug "WHOIS OK got user data"
        co_at = round(Time.to_sec(co_at))
        idle = round(Time.elapsed(pong_at, :sec))
        num311 = [u_nick, user, host, "*", ":"<>realname]
        num312 = [u_nick, IRCd.name, ":My IRCd is better than yours. \o/"]
        num317 = [u_nick, idle, co_at, ":seconds idle, signon time"]
        repl = [ snumeric(311, Enum.join(num311, " "), state), snumeric(312, Enum.join(num312, " "), state) ]
        if oper == true do
          repl = :lists.append(repl, snumeric(313, u_nick<>" :is a Server Administrator", state))
        end
        repl = :lists.append(repl, [
          snumeric(317, Enum.join(num317, " "), state), snumeric(318, u_nick<>" :End of /WHOIS list.", state),
        ])
        { :ok, repl, state }
      _ ->
        repl = [
          snumeric(401, nick<>" :No such nick", state),
          snumeric(318, nick<>" :End of /WHOIS list.", state)
        ]
        { :ok, repl, state }
    end
  end

  def handle_irc(:ping, [t], state) do
    state = state.pong_at(Time.now)
    { :ok, [ ":"<>IRCd.name<>" PONG "<>IRCd.name<>" :"<>t ], state }
  end

  def handle_irc(:pong, [t], state) do
    state = state.pong_at(Time.now)
    { :ok, state }
  end

  def handle_irc(:join, [channel], state) do
    cast_server(:join, [channel], state)
    { :ok, state }
  end

  def handle_irc(:kikoo, [], state) do
    { :ok, snotice("KIKOOO!", state), state }
  end

  def handle_irc(:oper, [login, passwd], state) do
    case List.keyfind(IRCd.operators, login, 0) do
      { _, oper_pass } ->
        if passwd == oper_pass do
          debug state, "became an IRCop."
          :gen_server.call({ :global, :server }, { :oper, self, state.uuid })
          repl = [
            ":"<>state.nick<>" MODE "<>state.nick<>" :+ao",
            snumeric("381", "IRCOP FLAG TURN ON", state),
          ]
          state = state.oper(true)
          { :ok, repl, state }
        else
          { :ok, snumeric("491", "hey little bastard, you forgot your password?", state), state }
        end
      _ ->
        { :ok, snumeric("491", "Only few mere mortals can enter the twilight zone.", state), state }
    end
  end

  def handle_irc(:kill, [nick], state), do: handle_irc(:kill, [nick, ""], state)
  def handle_irc(:kill, [nick, reason], state) do
    if reason == "", do: reason = "no reason given"
    if state.oper do
      :gen_server.cast({ :global, :server }, { :kill, state.uuid, nick, reason })
      { :ok, state }
    else
      { :ok, snumeric("481", "Permission denied — you're just a simple luser, not an awesome ircop.", state), state }
    end
  end

  # Ask for modes info
  def handle_irc(:mode, [target], state) do
    if String.first(target) == "#" do
      # Channel mode
      case :gen_server.call({ :global, :server }, { :get_channel_by_name, target }) do
        { :ok, { _, _, _, modes } } ->
          repl = [
            ":"<>IRCd.name<>" 324 "<>state.nick<>" "<>target<>" +"<>Enum.join(modes),
            ":"<>IRCd.name<>" 329 "<>state.nick<>" "<>target<>" 1379430610", # TODO: store chan creation time
          ]
          { :ok, repl, state }
        :fail -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" "<>target<>" :No such channel"], state }
      end
    else
      # user mode
      { :ok, [snotice("MODE on nicks not implemented yet", state)], state }
    end
  end

  def handle_irc(:mode, [target, "+b"], state), do: handle_irc(:mode, [target, "b"], state)
  def handle_irc(:mode, [target, "b"], state) do
    if String.first(target) == "#" do
      # Channel mode
      case :gen_server.call({ :global, :server }, { :get_channel_by_name, target }) do
        { :ok, { _, _, _, _ } } ->
          repl = [
          #":"<>IRCd.name<>" 324 "<>state.nick<>" "<>target<>" +"<>Enum.join(modes),
            ":"<>IRCd.name<>" 368 "<>state.nick<>" "<>target<>" :End of Channel Ban List", # TODO: store chan creation time
          ]
          { :ok, repl, state }
        :fail -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" "<>target<>" :No such channel"], state }
      end
    else
      # user mode
      { :ok, [":"<>IRCd.name<>" 501 "<>state.nick<>" :Unknown MODE flag"], state }
    end
  end

  # PRIVMSG
  def handle_irc(:privmsg, [target, text], state) do
    if String.first(target) == "#" do
      # Channel mode
      case :gen_server.call({ :global, :server }, { :get_channel_by_name, target }) do
        { :ok, { c_pid, _, _, _ } } ->
          :gen_server.cast(c_pid, {:privmsg, state.uuid, text})
          { :ok, state }
        :fail -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" "<>target<>" :No such channel"], state }
      end
    else
      # user mode
      case :gen_server.call({:global,:server}, { :get_user, target, 2 }) do
        { :ok, { u_pid, u_id, u_nick, u_modes } } ->
          msg = ":"<>get_mask(state)<>" PRIVMSG "<>u_nick<>" :"<>text
          if u_pid == self do
            send(state.socket, msg)
          else
            :gen_server.cast(u_pid, { :send, msg })
          end
          { :ok, state }
        _ -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" :No such nick"], state }
      end
    end
  end

  # PART
  def handle_irc(:part, [target], state), do: handle_irc(:part, [target, "(no reason given)"], state)

  def handle_irc(:part, [target, reason], state) do
    case :gen_server.call({ :global, :server }, { :get_channel_by_name, target }) do
      { :ok, { c_pid, _, _, _ } } ->
        state = state.channels(List.keydelete(state.channels, c_pid, 0))
        :gen_server.cast(c_pid, {:part, state.uuid, reason})
        { :ok, state }
      :fail -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" "<>target<>" :No such channel"], state }
    end
  end

  # QUIT
  def handle_irc(:quit, [], state), do: handle_irc(:quit, ["Client Quit"], state)

  def handle_irc(:quit, [reason], state) do
    unless reason == "Client Quit", do: reason = "Quit: "<>reason
    :gen_server.cast({ :global, :server }, { :quit, state.uuid,reason })
    { :ok, state }
  end

  # WHO
  def handle_irc(:who, [target], state) do
    if String.first(target) == "#" do
      case :gen_server.call({ :global, :server }, { :get_channel_by_name, target }) do
        { :ok, { c_pid, _, _, _ } } ->
          :gen_server.cast(c_pid, { :who, target, state.uuid })
          { :ok, state }
        :fail -> { :ok, [":"<>IRCd.name<>" 403 "<>state.nick<>" "<>target<>" :No such channel"], state }
      end
    else
      { :ok, [snotice("WHO on nicks not implemented yet", state)], state }
    end
  end
  def handle_irc(:debug, [action], state) do
    aktion = case action do
      "printstate" -> :printstate
      _ -> :printstate
    end
    :gen_server.call({ :global, :server }, { aktion })
    { :ok, "NOTICE " <> state.nick <> " :ok", state }
  end

  def handle_irc(action, args, state) do
    debug state, "Unhandled IRC action: " <> inspect(action) <> " " <> inspect(args)
    { :ok, state }
  end

  # ---- Termination
  def terminate(reason, state) do
    debug state, "User closed: " <> inspect(reason)
    if state.socket, do: send(state.socket, "ERROR :Closing Link: Terminated (" <> inspect(reason) <> ")")
    :ok
  end

  def code_change(_old, state, _extra), do: { :ok, state }

  # ---- Communication
  defp send(socket, action, args), do: debug "todo send"

  defp send(socket, data) when is_list(data) do
    send(socket, Enum.join(List.flatten(data), "\r\n"))
  end

  defp send(socket, data) when is_binary(data) do
    debug " <<<< " <> inspect(data)
    :gen_tcp.send(socket, data<>"\r\n")
  end

  # ---- Log
  defp debug(state, string) do
    str = "[UUID:" <> inspect(state.uuid) <> "] " <> string
    debug(str)
  end

  defp debug(string), do: Lager.info inspect(self) <> "[IRCd.User] " <> string
end
