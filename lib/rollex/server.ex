defmodule Rollex.Server do
  use GenServer.Behaviour

  defrecord ServerInfo,
    uri: "",
    socket: nil

  defrecord Config,
    user: nil,
    channels: [],
    workers: [Rollex.IRC.Worker.new.module(Rollex.Worker.Pong).interests([:ping])]

  defrecord State,
    server: nil,
    config: nil,
    irc_sup: nil,
    worker_sup: nil

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
    {:ok, pid} = :supervisor.start_child(state.irc_sup, wsup)

    state = Enum.map(state.config.workers,
      &start_worker(&1, pid)) |> state.config.workers |> state.config
    state = state.worker_sup(pid)
    #self <- {:start_processing, state}
    state.config.user |> Rollex.IRC.login(state.server)
    process(state, [])
    {:noreply, state}
  end

  # TODO: This is unused at the moment. Remove or use instead of
  # process in the above function (handle_info)
  def handle_info({:start_processing, state}, _s) do
    IO.puts "Starting to read messages from server"
    process(state, [])
    {:noreply, state}
  end

  @doc """
  Starts a Worker process
  """
  def start_worker(Rollex.IRC.Worker[interests: []], _pid) do :ok end
  def start_worker(worker, pid) do
    wcfg = Supervisor.Behaviour.worker(worker.module, [], [])
    {:ok, pid} = :supervisor.start_child(pid, wcfg)
    worker.pid(pid)
  end

  @doc """
  Connect to the serveruri with a default configuration
  """
  def connect(serveruri) do
    connect(serveruri,
      Config.new.user(Rollex.IRC.User.new).channels(["#rollex"]))
  end

  @doc """
  Connect to the serveruri with specified config
  """
  def connect(serveruri, config) do
    {:ok, sock} = Socket.connect(serveruri)
    Socket.options!(sock, packet: :line)
    #{:ok, sock} = Socket.SSL.connect("chat.freenode.net", 6697, packet: :line) #Socket.connect(serveruri)
    {:ok, State.new.server(
      ServerInfo.new.uri(serveruri).socket(sock)).config(config)}
  end

  @doc """
  Parses lines from the irc server socket into an IRC.Command and
  then sends the command to all interested Workers
  """
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

  @doc """
  Is the IRC.Worker interested in the message type or not
  """
  def interested?(Rollex.IRC.Worker[interests: [:all]], _t) do true end
  def interested?(Rollex.IRC.Worker[interests: interests], type) do
    Enum.member?(interests, type)
  end
  def interested?(_w, _t) do false end

  @doc """
  Sends an IRC.Command record to a Worker process
  """
  def worker_command(worker, command, state) do
    cond do
      worker.pid != nil ->
        IO.puts "Sending command: " <> command.raw <> " to a worker"
        :gen_server.cast(worker.pid, {command, state})
      worker.pid == nil ->
        :ok
    end
  end

  

  @doc """
  Converts a raw IRC message string to a IRC.Command record
  """
  def inbound(raw) do
    cmd = Rollex.IRC.Command.new.raw(raw).user(
      Rollex.IRC.User.new.name("").nick(""))
    tokens = raw |> String.split
    cond do
      Rollex.IRC.prefixed? raw ->
        cmd = tokens |> Enum.first |> Rollex.IRC.get_user |> cmd.user
        cmd = tokens |> Enum.at(1) |> Rollex.IRC.command_type |> cmd.type
        tokens |> Enum.drop(2) |> cmd.params
      true ->
        cmd = tokens |> Enum.first |> Rollex.IRC.command_type |> cmd.type
        tokens |> Enum.drop(1) |> cmd.params
    end
  end
end
