defmodule Rollex.IRC do
  use GenServer.Behaviour

  defrecord ServerInfo,
    uri: "",
    socket: nil

  defrecord Worker,
    module: nil,
    interests: [],
    pid: nil

  defrecord Config,
    user: nil,
    channels: [],
    workers: [Worker.new.module(Rollex.Worker.Pong).interests([:ping])]

  defrecord State,
    server: nil,
    config: nil,
    irc_sup: nil,
    worker_sup: nil

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

  def init([spid, uri]) do
    {:ok, state} = connect(uri)
    state = spid |> state.irc_sup
    self <- {:start_worker_sup}
    {:ok, state}
  end

  def handle_info({:start_worker_sup}, state) do
    IO.puts "Starting worker supervisor and all workers"

    wsup = Supervisor.Behaviour.supervisor(
      Rollex.Worker.Supervisor, [],
      restart: :permanent)
    {:ok, pid} = :supervisor.start_child(state.irc_sup, wsup) # was: start_child(spid, wsup)
    IO.puts "Started worker supervisor"

    state = Enum.map(state.config.workers,
      &start_worker(&1, pid)) |> state.config.workers |> state.config
    state = state.worker_sup(pid)
    IO.inspect state
    #self <- {:start_processing, state}
    state.config.user |> login(state.server)
    process(state, [])
    {:noreply, state}
  end

  def handle_info({:start_processing, state}, _s) do
    IO.puts "Starting to read messages from server"
    process(state, [])
    {:noreply, state}
  end

  def start_worker(Worker[interests: []], _pid) do :ok end
  def start_worker(worker, pid) do
    wcfg = Supervisor.Behaviour.worker(worker.module, [], [])
    {:ok, pid} = :supervisor.start_child(pid, wcfg)
    worker.pid(pid)
  end

  def connect(serveruri) do
    connect(serveruri,
      Config.new.user(User.new).channels(["#vorce"]))
  end

  def connect(serveruri, config) do
    {:ok, sock} = Socket.connect(serveruri)
    Socket.options!(sock, packet: :line)
    #{:ok, sock} = Socket.SSL.connect("chat.freenode.net", 6697, packet: :line) #Socket.connect(serveruri)
    {:ok, State.new.server(
      ServerInfo.new.uri(serveruri).socket(sock)).config(config)}
  end

  def process(irc_state, lines) when is_list(lines) do
    case irc_state.server.socket |> Socket.Stream.recv do
      {:error, message} ->
        {:error, message, lines}
      {:ok, line} ->
        IO.puts "Raw: " <> line
        #line |> inbound |> process irc_state
        command = line |> inbound
        interested_workers = irc_state.config.workers |>
          Enum.filter(&interested?(&1, command.type))
        interested_workers |> Enum.each(&worker_command(&1, command, irc_state))
        process(irc_state, [line|lines] |> Enum.take 500)
      eof ->
        {:error, eof, lines}
    end
  end

  def interested?(Worker[interests: [:all]], _t) do true end
  def interested?(Worker[interests: interests], type) do
    Enum.member?(interests, type)
  end
  def interested?(_w, _t) do false end

  def worker_command(worker, command, state) do
    cond do
      worker.pid != nil ->
        IO.puts "Sending command: " <> command.raw <> " to a worker"
        :gen_server.cast(worker.pid, {command, state})
      worker.pid == nil ->
        :ok
    end
  end

  def prefixed?(":" <> _rest) do true end
  def prefixed?(_nope) do false end

  def pong(server, params) do
    irc_command(:pong, params) |> send(server)
  end

  @doc """
  Creates a new IRC.Command of (atom) type with params
  """
  def irc_command(type, params) when is_atom(type) do
    Command.new.type(type).params(params)
  end

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
  Converts a raw IRC message string to a IRC.Command record
  """
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
    irc_command(:nick, [user.nick]) |> send(server)
    irc_command(:user, [user.name, user.nick, "0", ":" <> user.realname]) |> send(server)
  end

  def test() do
    Socket.Manager.start(nil, nil)
    :ssl.start()
    uri = "ssl://donger.alphajanne.com:6697"
    {:ok, state} = Rollex.IRC.connect(uri)
    User.new |> login(state.server)
    #state
    Rollex.IRC.process(state, [])
  end

  def test2() do
    Socket.Manager.start(nil, nil)
    :ssl.start()
    uri = "ssl://donger.alphajanne.com:6697"
    { :ok, pid } = :gen_server.start_link(Rollex.IRC, [self, uri], [])
    #:gen_server.cast(pid, {})
  end
end
