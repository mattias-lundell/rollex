defmodule Rollex.IRC do
  use GenServer.Behaviour

  defrecord ServerInfo,
    uri: "",
    socket: nil

  defrecord RollexConfig,
    user: nil,
    channels: []

  defrecord IRCState,
    server: nil,
    config: nil,
    logged_in: false

  defrecord User,
    name: "rollex",
    nick: "rollex",
    realname: "Rollex the IRC gentlebot"

  defrecord Command,
    user: nil,
    type: :unknown,
    params: [],
    raw: ""

  def start_link(args) do
    :gen_server.start_link(__MODULE__, args, [])
  end

  def init(uri) do
    {:ok, state} = connect(uri, ["#vorce"])
    process state, []
  end

  def connect(serveruri) do
    connect(serveruri,
      RollexConfig.new.user(
        User.new).channels(["#vorce"]))
  end

  def connect(serveruri, config) do
    {:ok, sock} = Socket.connect(serveruri)
    Socket.options!(sock, packet: :line)
    #{:ok, sock} = Socket.SSL.connect("chat.freenode.net", 6697, packet: :line) #Socket.connect(serveruri)
    {:ok, IRCState.new.server(
      ServerInfo.new.uri(serveruri).socket(sock)).config(config)}
  end

  def process(irc_state, lines) when is_list(lines) do
    case irc_state.server.socket |> Socket.Stream.recv do
      {:error, message} ->
        {:error, message, lines}
      {:ok, line} ->
        #data |> String.split("\r\n") |> Enum.each(&process_line(&1))
        IO.puts "Raw: " <> line
        irc_state = line |> inbound |> process irc_state
        process(irc_state, [line|lines] |> Enum.take 500)
      eof ->
        {:error, eof, lines}
    end
  end

  def process(Command[type: :notice, params: ["Auth"|_tail]], IRCState[logged_in: false] = state) do
    state.config.user |> login(state.server)
    state.logged_in(true)
  end

  def process(Command[user: user, type: :ping], state) do
    IO.puts "Received PING from: " <> user.name
    pong(state.server, [user.name])
    state
  end

  def process(cmd, state) when is_record(cmd) do
    IO.puts "Unknown command received: " <> cmd.raw
    state
  end
  
  def prefixed?(":" <> _rest) do true end
  def prefixed?(_nope) do false end

  def channel_message?(_line) do false end

  def pong(server, params) do
    irc_command("PONG", [params]) |> send(server)
  end

  def irc_command(command, params) do
    Command.new.type(command).params(params)
  end

  def send(command, server) do
    cmd = command |> outbound
    IO.puts "Sending command: " <> cmd
    Socket.Stream.send(server.socket, cmd)
  end

  def outbound(command) do
    Enum.join([command.type] ++ command.params, " ") <> "\r\n"
  end

  def inbound(raw) do
    cmd = Command.new.raw(raw).user(User.new.name("").nick(""))
    tokens = raw |> String.split
    cond do
      prefixed? raw ->
        cmd = tokens |> Enum.first |> get_user |> cmd.user
        cmd = tokens |> Enum.at(1) |> command_type |> cmd.type
        tokens |> Enum.drop(2) |> cmd.params
      true ->
        cmd = tokens |> Enum.first |> command_type |> cmd.type
        tokens |> Enum.drop(1) |> cmd.params
    end
  end

  @doc """
    Returns a command type from its string, ex:
    command_type("PING") returns :ping
  """
  def command_type(command_string) do
    command_string |> String.downcase |> binary_to_atom
  end

  # ":sherlockj!~sherlock_@host.isp.yeah.com"
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

  def login(user, server) when is_record(user) do
    irc_command("NICK", [user.nick]) |> send(server)
    irc_command("USER", [user.name, user.nick, "0", ":" <> user.realname]) |> send(server)
  end

  def test() do
    Socket.Manager.start(nil, nil)
    :ssl.start()
    server = "ssl://donger.alphajanne.com:6697"
    {:ok, state} = Rollex.IRC.connect(server)
    #state
    Rollex.IRC.process(state, [])
  end
end
