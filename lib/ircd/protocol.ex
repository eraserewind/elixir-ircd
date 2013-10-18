defmodule IRCd.Protocol do
  @moduledoc """
  IRC helper functions.
  Inspired from irc.el from various places.

  RFC 2812 - http://tools.ietf.org/html/rfc2812
  """
  require Lager

  # Enable debug log?
  defp use_debug, do: false

  # NUMERICS
  #def RPL_WELCOME, do: '001'
  #def RPL_YOURHOST, do: '002'
  #def RPL_CREATED, do: '003'
  #def RPL_MYINFO, do: '004'
  #def RPL_BOUNCE, do: '005'
  #def RPL_USERHOST, do: '302'
  #def RPL_ISON, do: '303'

  def rpl(key) do
    case key do
      :welcome -> "001"
      :yourhost -> "002"
      :created -> "003"
      :myinfo -> "004"
      :bounce -> "005"
      :userhost -> "302"
      :ison -> "303"
      :away -> "301"
      :unaway -> "305"
      :nowaway -> "306"
      :whoisuser -> "311"
      :whoisserver -> "312"
      :whoisoperator -> "313"
      :whowasuser -> "314"
      :endofwho -> "315"
      :whoisidle -> "317"
      :endofwhois -> "318"
      :whoischannels -> "319"
      :liststart -> "321" # Obselete!
      :list -> "322"
      :listend -> "323"
      :uniqopis -> "325"
      :channelmodeis -> "324"
      :notopic -> "331"
      :topic -> "332"
      :inviting -> "341"
      :summoning -> "342"
      :invitelist -> "346"
      :endofinvitelist -> "347"
      :exceptlist -> "348"
      :endofexceptlist -> "349"
      :version -> "351"
      :whoreply -> "352"
      :namreply -> "353"
      :endofnames -> "366"

      _ -> "000"
    end
  end

  # Parse an IRC command
  # Returns:
  # { :ok, { action, args } }
  # { :error }
  def parse_input(data) do
    { :ok, string } = String.from_char_list(data)
    string = String.strip(string)
    debug "parse_input >>> " <> string

    # 1 : Extract command
    # FIXME: maybe this can be a target ?? (:user COMMAND ..) ? (not sure for clients)
    case String.split(string, " ", global: false) do
      [ command | rest ] -> string = Enum.join(rest)
      [ command ] -> string = "" # this should almost never happen
    end
    debug "command = " <> inspect(command)

    # 2: Extract final arg
    debug "step 2 string " <> inspect(string)
    case String.split(string, ":", global: false) do
      [ args | final ] -> string = args
      [ args ] -> string = args
    end
    debug "final = " <> inspect(final)

    debug "step 3 string " <> inspect(string)
    unless string == "" do
      string = String.strip(string)
      # 3: Extract args
      args = String.split(string, " ")
      debug " args = " <> inspect(args)
    else
      debug "to hell with step 3, string is empty"
      args = []
    end

    if final do
      args = :lists.append(args, final)
      debug " final args = " <> inspect(args)
    end

    { :ok, command, args }
  end

  def format_numeric(sender, numeric, target, text) when is_atom(numeric) do
    format_numeric(sender, rpl(numeric), target, text)
  end

  def format_numeric(sender, numeric, target, text) do
    if is_integer(numeric), do: numeric = inspect(numeric)
    unless String.contains?(text, ":"), do: text = ":"<>text
    ":" <> sender <> " " <> numeric <> " " <> target <> " " <> text
  end

  def format_out(sender, action, text) do
    ":" <> Enum.join([sender,action,text], " ")
  end

  defp debug(string), do: (if use_debug, do: (Lager.info inspect(self) <> " IRCd.Protocol: " <> string))
end
