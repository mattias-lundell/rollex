defmodule Rollex.IRC do
  defrecord Worker,
    module: nil,
    interests: [],
    pid: nil

  defrecord User,
    name: "rollex",
    nick: "rollex",
    realname: "Rollex the IRC gentlebot"

  defrecord Command,
    user: nil,
    type: :unknown,
    params: [],
    raw: ""

  @doc """
  Is the string prefixed with an ":"?
  """
  def prefixed?(":" <> _rest) do true end
  def prefixed?(_nope) do false end

  @doc """
  Sends a PONG to the irc server. TODO: Should this be moved to the
  pong worker?
  """
  def pong(server, params) do
    command(:pong, params) |> send(server)
  end

  @doc """
  Creates a new IRC.Command of (atom) type with params
  """
  def command(type, params) when is_atom(type) do
    Command.new.type(type).params(params)
  end

  @doc """
  sends an IRC.Command to the irc server
  """
  def send(command, server) do
    cmd = command |> outbound
    IO.puts "Sending command: " <> cmd
    Socket.Stream.send(server.socket, cmd)
  end

  @doc """
  Converts an IRC.Command record to a string suitable for sending
  to the IRC server.
  """
  def outbound(command) do
    type_str = command.type |> atom_to_binary |> String.upcase
    Enum.join([type_str] ++ command.params, " ") <> "\r\n"
  end

  @doc """
    Returns a command type from its string, ex:
    command_type("PING") returns :ping
  """
  def command_type(command_string) do
    command_string |> String.downcase |> binary_to_atom
  end

  @doc """
  Creates an IRC.User record from an irc user string
  (Ex: ":sherlockj!~sherlock_@host.isp.yeah.com")
  """
  def get_user(":" <> raw_user) do
    result = raw_user |> String.split %r"!|@"
    cond do
      Enum.count(result) == 1 ->
        name = Enum.first result
        User.new.name(name).nick(name)
      true ->
        User.new.nick(Enum.first result).name(Enum.at result, 1)
    end
  end

  @doc """
  sends login information to the irc server (NICK and USER)
  """
  def login(user, server) when is_record(user) do
    command(:nick, [user.nick]) |> send(server)
    command(:user, [user.name, user.nick, "0", ":" <> user.realname]) |> send(server)
  end
end

