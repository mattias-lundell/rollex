defmodule RollexTest do
  use ExUnit.Case

  setup_all do
    Socket.Manager.start(nil, nil)
    :ssl.start()
  end

  test "should connect to an SSL IRC server" do
    server = "ssl://chat.freenode.net:6697"
    {reply, _s} = Rollex.Server.connect(server)
    assert reply == :ok
  end

  test "should parse inbound server message" do
    msg = ":drop.disrespect.com NOTICE Auth :*** Looking up your hostname"
    cmd = msg |> Rollex.Server.inbound

    assert cmd.user.name == "drop.disrespect.com"
    assert cmd.type == :notice
    assert cmd.params == String.split("Auth :*** Looking up your hostname")
  end

  test "should create string for sending" do
    type = "PONG"
    from = ":some.server.com"
    cmd = type |> Rollex.IRC.command_type |>
      Rollex.IRC.command([from])

    assert cmd |> Rollex.IRC.outbound == type <> " " <> from <> "\r\n"
  end

  test "should parse inbound raw message to record" do
    raw = "PING :server.host.com"
    cmd = Rollex.Server.inbound(raw)

    assert is_record(cmd)
    assert cmd.type == :ping
    assert cmd.user != nil
    assert cmd.user.name != nil
    assert cmd.user.nick != nil
    assert cmd.params == [":server.host.com"]
  end

  test "should create irc command of requested type" do
    params = [":hello.there"]
    cmd = Rollex.IRC.command(:pong, params)

    assert is_record(cmd)
    assert cmd.type == :pong
    assert cmd.params == params
  end

  test "should get user from raw user string" do
    raw_user = ":sherlockj!~sherlock_@host.isp.yeah.com"
    user = Rollex.IRC.get_user(raw_user)

    assert is_record(user)
    assert user.name == "~sherlock_"
    assert user.nick == "sherlockj"
  end

  test "should start pong worker" do
    {:ok, spid} = :supervisor.start_link(Rollex.Server.Supervisor, [])
    ponger = Rollex.IRC.Worker.new.module(Rollex.Worker.Pong).interests(
      [:ping]) |> Rollex.Server.start_worker(spid)

    assert Rollex.Server.worker_command(ponger,
      Rollex.IRC.command(:ping, [":foo.bar"]),
      Rollex.Server.State.new)
  end

  test "should load default configuration" do
    cfg = Rollex.Config.file! "example.config.exs"

    assert cfg.servers == ["ssl://chat.freenode.net:6697"]
    assert cfg.channels == ["#rollex"]
    assert Enum.count(cfg.workers) == 1
    assert is_record(Enum.first(cfg.workers))
    assert is_record(cfg.user)
    assert cfg.user.nick == "rollex"
    assert cfg.user.name == "rollex"
  end
end
